t0 <- proc.time()

parser <- optparse::OptionParser(
  usage = "usage: grid.R [options] db vct_dir out_prefix",
  description = "Create gridded distribution data from VCT data"
)
parser <- optparse::add_option(parser, "--abund-csv",
  type = "character", default = "", metavar = "FILE",
  help = "Optional abundane calibration CSV file"
)
parser <- optparse::add_option(parser, "--bin-count",
  type = "integer", default = 30, metavar = "N",
  help = "Number of bins along each dimension of the distribution [default %default]",
)
parser <- optparse::add_option(parser, "--dimensions",
  type = "character", default = "fsc_small,pe,chl_small,Qc,diam",
  help = "Comma-separated list of dimensions for grid. Qc will always be included regardless of this value. [default %default]"
)
parser <- optparse::add_option(parser, "--keep-outliers",
  action = "store_true", default = FALSE,
  help = "Don't remove 3 minute windows flagged as outliers [default %default]"
)
parser <- optparse::add_option(parser, "--model",
  action = "store_true", default = FALSE,
  help = "Prepare QC gridded data for MPM model, i.e. adjust the grid bin size so that (1 / log2(bin_size)) is an integer"
)
parser <- optparse::add_option(parser, "--no-data.table",
  action = "store_true", default = FALSE,
  help = "Don't use data.table for performance-critical aggregation [default %default]"
)
parser <- optparse::add_option(parser, "--par-csv",
  type = "character", default = "",
  help = "Optional PAR calibration values CSV file. [default %default]"
)
parser <- optparse::add_option(parser, "--pop",
  type = "character", default = "",
  help = "Optional single population filter. Default is to include all populations."
)
parser <- optparse::add_option(parser, "--processes",
  type = "integer", default = 1, metavar = "N",
  help = "Number of processes to use [default %default]",
)
parser <- optparse::add_option(parser, "--psd-mode",
  action = "store_true", default = FALSE,
  help = "Create particle size distribution file for hourly data (only pop, Qc binning, Qc_sum/uL values) [default %default]"
)
parser <- optparse::add_option(parser, "--quantile",
  type = "character", default = "50", metavar = "QUANTILE",
  help = "Quantile. Choices are 2.5, 50, 97.5. [default %default]",
)
parser <- optparse::add_option(parser, "--range-diam",
  type = "character", default = "0.1,37",
  help = "Diameter grid range as two real numbers separated by a comma [default %default]"
)
parser <- optparse::add_option(parser, "--range-fsc-pe-chl",
  type = "character", default = "1,3200",
  help = "FSC/PE/CHL grid range as two real numbers separated by a comma [default %default]"
)
parser <- optparse::add_option(parser, "--range-Qc",
  type = "character", default = "0.008140889,0.260508439",
  help = "Qc grid range as two real numbers separated by a comma [default %default]"
)
parser <- optparse::add_option(parser, "--refrac-csv",
  type = "character", default = "", metavar = "FILE",
  help = "Optional per-population refractive index CSV file"
)
parser <- optparse::add_option(parser, "--volume",
  type = "integer", default = NULL, metavar = "VOLUME",
  help = "Hard code a single volume value for abundance calculations, i.e. if stream pressure or file duration is unreliable [default %default]",
)

p <- optparse::parse_args2(parser)
if (length(p$args) < 3) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- normalizePath(p$args[1])
  vct_dir <- normalizePath(p$args[2])
  out_prefix <- p$args[3]
  abund_csv <- p$options$abund_csv
  bin_count <- p$options$bin_count
  dimensions <- p$options$dimensions
  keep_outliers <- p$options$keep_outliers
  model <- p$options$model
  no_data.table <- p$options$no_data.table
  par_csv <- p$options$par_csv
  pop <- p$options$pop
  processes <- p$options$processes
  psd_mode <- p$options$psd_mode
  quantile_ <- p$options$quantile
  range_diam_string <- p$options$range_diam
  range_fsc_pe_chl_string <- p$options$range_fsc_pe_chl
  range_Qc_string <- p$options$range_Qc
  refrac_csv <- p$options$refrac_csv
  volume <- p$options$volume

  # Check for file / dir paths to be read
  if (!dir.exists(vct_dir) || !file.exists(db)) {
    stop(paste0("vct_dir or db does not exist"))
  }

  # Adjust CSV empty value
  if (abund_csv == "") {
    abund_csv <- NULL
  }
  if (par_csv == "") {
    par_csv <- NULL
  }
  if (refrac_csv == "") {
    refrac_csv <- NULL
  }

  # Set up dimensions for grid
  possible_dimensions <- unlist(stringr::str_split("fsc_small,pe,chl_small,Qc,diam", ","))
  dimensions <- unlist(stringr::str_split(dimensions, "\\s*,\\s*"))  # accept and remove spaces in comma-sep list
  if (length(setdiff(dimensions, possible_dimensions)) > 0) {
    stop(glue("Error: unknown dimensions {stringr::str_flatten(setdiff(dimensions, possible), ', ')}"))
  }
  # Always add Qc
  if (! "Qc" %in% dimensions) {
    dimensions[length(dimensions) + 1] <- "Qc"
  }
  # If PSD is requested, only use Qc
  if (psd_mode) {
    dimensions <- "Qc"
  }

  # Parse grid ranges
  parse_range_string <- function(range_string) {
    parts <- stringr::str_split(range_string, ",", simplify = TRUE)
    parts <- suppressWarnings(as.numeric(parts))
    return(parts)
  }

  check_parsed_range <- function(range_vec, dim_name) {
    if (length(range_vec) != 2) {
      stop("incorrect field count in '", dim_name, "' range string, expeced 2, got ", length(range_vec))
    }
    if (any(is.na(range_vec))) {
      stop("received non-numeric input in '", dim_name, "' range string")
    }
  }

  range_diam <- parse_range_string(range_diam_string)
  range_fsc_pe_chl <- parse_range_string(range_fsc_pe_chl_string)
  range_Qc <- parse_range_string(range_Qc_string)
  check_parsed_range(range_diam)
  check_parsed_range(range_fsc_pe_chl)
  check_parsed_range(range_Qc)

  # Handle empty pop
  if (pop == "") {
    pop <- NULL
  }

  # Hard-coded configuration
  # pop <- "prochloro"
  # qc_range <- c(0.008140889,  0.260508439)  # to match 2022-09 Qc gridded data
  # qc_range <- c(0.002,  64)
  # diam_range <- c(0.1, 37)
  # channel_range <- c(1, 3200)
}

message("using popcycle version ", packageVersion("popcycle"))

library(dplyr, warn.conflicts=FALSE)

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

get_delta_v_int_qc_range <- function(qc_range, bins) {
  # The growth model takes 1 / (log2 distance between bins) as an integer. Calculate the end of
  # the grid range as the closest such integer that creates a grid that contains the true grid range
  # from the previous step
  delta_log2 <- diff(seq(from=log2(qc_range[1]), to=log2(qc_range[2]), length=bins+1))[1]
  delta_log2_inv <- 1 / delta_log2
  delta_log2_inv_int <- as.integer(delta_log2_inv)
  # Now original range should be
  # c(qc_range_orig[1], 2**(log2(qc_range_orig[1]) + (bins * (1 / delta_log2_inv))))
  # Expressing delta_log2_inv as an int gives a little headroom at the top end
  result <- list(
    qc_range = c(
      qc_range[1],
      2**(log2(qc_range[1]) + (bins * (1 / delta_log2_inv_int)))
    ),
    delta_log2 = delta_log2,
    delta_log2_inv = delta_log2_inv,
    delta_log2_inv_int = delta_log2_inv_int
  )
  return(result)
}


# ----------
# Begin main
# ----------
dated_msg("Start")
message("Configuration:")
message("--------------")
message("db = ", db)
message("vct-dir = ", vct_dir)
message("abund-csv = ", abund_csv)
message("bin_counts = ", bin_count)
message("keep-outliers = ", keep_outliers)
message("model = ", model)
message("no-data.table = ", no_data.table)
message("out_prefix = ", out_prefix)
message("par-csv = ", par_csv)
message("pop = ", pop)
message("processes = ", processes)
message("psd-mode = ", psd_mode)
message("quantile = ", quantile_)
message("range-diam = ", paste(range_diam, collapse=", "))
message("range-fsc-pe-chl = ", paste(range_fsc_pe_chl, collapse=", "))
message("range-Qc = ", paste(range_Qc, collapse=", "))
message("refrac-csv = ", refrac_csv)
message("volume = ", volume)

# Create output file names
grid_bins_out <- paste0(out_prefix, ".grid_bins.parquet")
gridded_out <- paste0(out_prefix, ".gridded.parquet")
volume_out <- paste0(out_prefix, ".volume.parquet")

hourly_gridded_out <- paste0(out_prefix, ".hourly_gridded.parquet")
hourly_gridded_PSD_out <- paste0(out_prefix, ".hourly_PSD.parquet")
hourly_volume_out <- paste0(out_prefix, ".hourly_volume.parquet")
par_out <- paste0(out_prefix, ".par.parquet")
hourly_par_out <- paste0(out_prefix, ".hourly_par.parquet")
stat_out <- paste0(out_prefix, ".db_stat.parquet")

cruise_ <- popcycle::get_cruise(db)
message("cruise = ", cruise_)

vct_files <- list.files(vct_dir, "\\.parquet$", full.names = TRUE)
dated_msg("found ", length(vct_files), " VCT hourly files")

sfl_tbl <- popcycle::get_sfl_table(db)
if (!keep_outliers) {
  flagged_dates <- sfl_tbl %>%
    dplyr::filter(flag != 0) %>%
    dplyr::pull(date)
  sfl_tbl <- sfl_tbl %>%
    dplyr::filter(flag == 0)
  dated_msg(length(flagged_dates), " flagged dates")
} else {
  flagged_dates <- NULL
}

# Get PAR
par <- sfl_tbl %>%
  dplyr::select(date, par, lat, lon)

# Correct raw PAR values
dated_msg("Reading PAR calibration CSV file ", par_csv)
par_calib <- popcycle::read_par_csv(path = par_csv) %>%
  dplyr::filter(!is.na(correction), !is.infinite(correction), cruise == cruise_)
if (nrow(par_calib) == 1) {
  dated_msg("Applying PAR correction value ", par_calib$correction)
  par$par <- par$par * par_calib$correction[1]
} else {
  dated_msg("No PAR correction value found for this cruise")
}

# Create metadata table
meta_full <- popcycle::create_meta(db, as.numeric(quantile_))
meta <- meta_full[, c("date", "volume", "opp_evt_ratio", "flag")]
if (! is.null(volume)) {
  dated_msg(glue::glue("setting volume to {volume} in meta table"))
  meta$volume <- volume
}
if (!keep_outliers) {
  meta <- meta %>% dplyr::filter(flag == 0)
}
# Use median(opp_evt_ratio) with outliers retained for virtualcore volume
# calc, as in popcycle::create_stat_table
median_opp_evt_ratio_full <- median(meta_full$opp_evt_ratio)
median_opp_evt_ratio <- median(meta$opp_evt_ratio)
dated_msg("full opp/evt = ", median_opp_evt_ratio_full)
dated_msg("no-outlier opp/evt = ", median_opp_evt_ratio)
ratio <- median_opp_evt_ratio_full
dated_msg("using ratio = ", ratio)
volume <- popcycle::create_volume_table(
  meta, time_expr = NULL, median_opp_evt_ratio = ratio
)
hourly_volume <- popcycle::create_volume_table(
  meta, time_expr = "1 hour", median_opp_evt_ratio = ratio
)

# Get stat table
stat <- popcycle::get_stat_table(db)

# Choose indices of refraction
dated_msg("Reading indices of refraction CSV file ", refrac_csv)
refracs <- popcycle::read_refraction_csv(path = refrac_csv) %>%
  dplyr::filter(cruise == cruise_) %>%
  select(-c(cruise))
message(paste0(capture.output(refracs), collapse="\n"))
refracs$cruise <- NULL
if (nrow(refracs) == 0) {
  dated_msg(paste0("refractive index table has no entry for ", cruise, ", using mid for Pro/Syn, lwr for Pico/Croco/Beads/Unknown"))
  refracs <- tibble::as_tibble(data.frame(
    prochloro = "mid", synecho = "mid", croco = "lwr", picoeuk = "lwr", beads = "lwr", unknown = "lwr"
  ))
  message(paste0(capture.output(refracs), collapse="\n"))
}
if (nrow(refracs) > 1) {
  stop(paste0("refractive index table has multiple entries for ", cruise))
}

dated_msg("Retrieving influx-based abundance calibration file ", abund_csv)
calib <- popcycle::read_calib_csv(path = abund_csv) %>%
  dplyr::filter(cruise == cruise_)
if (length(unique(calib$cruise)) == 0) {
  dated_msg(paste0("calibration table has no entry for ", cruise_, ", no calibration will be applied"))
  calib <- NULL
} else {
  message(paste0(capture.output(calib), collapse="\n"))
}

# Create output directory
dir.create(dirname(out_prefix), recursive = TRUE, showWarnings = FALSE)

# Grid
dated_msg("Creating grid with dimensions = ", paste(dimensions, collapse = ", "))
if (model) {
  v_int_qc_range <- get_delta_v_int_qc_range(range_Qc, bin_count)
  dated_msg("v_int_qc_range")
  message(paste0(capture.output(v_int_qc_range), collapse="\n"))
  range_Qc_tmp <- v_int_qc_range$qc_range
} else {
  range_Qc_tmp <- range_Qc
}

# To match 2022-09 Qc gridded for validation
# qc_range <- c(0.008140889, 0.260508439)
dated_msg("Qc_range = ", paste(range_Qc_tmp, collapse=", "))
grid_bins <- popcycle::create_grid_bins(
  bin_count, log_base=2, log_answers=FALSE,
  Qc_range = range_Qc_tmp,
  diam_range = range_diam,
  channel_range = range_fsc_pe_chl
)
grid_bins <- grid_bins[dimensions]
grid_bins_df <- tibble::as_tibble(grid_bins)
grid_bins_df <- grid_bins_df %>% tibble::add_column(cruise = cruise_, .before=1)
# Save grid to file
dated_msg("Writing grid bins file to ", grid_bins_out)
arrow::write_parquet(grid_bins_df, grid_bins_out)

# Make gridded data
dated_msg("Creating gridded data")
ptm <- proc.time()
gridded <- popcycle::create_gridded(
  vct_files, quantile_, refracs, grid_bins, ignore_dates = flagged_dates, pop = pop,
  cores = processes, use_data.table = !no_data.table
)

invisible(gc())
deltat <- proc.time() - ptm
dated_msg("Created full gridded data in ", deltat[["elapsed"]], " seconds")

# No data check
if (nrow(gridded) == 0 || all(is.na(gridded$date))) {
  # No data for this cruise
  dated_msg("No data for ", cruise_)
  dated_msg("Writing empty output files")
  file.create(
    gridded_out,
    hourly_gridded_out,
    paste0(out_prefix, ".full_par.parquet"),
    paste0(out_prefix, ".hourly_par.parquet")
  )
  quit(save="no")
}

# Remove counts out of grid range (coord is NA)
na_count <-sum(is.na(gridded))
if (na_count) {
  stop(na_count, " out-of-range values")
}

# Hourly data
# data.table multi-threading temporarily disabled for grouping
orig_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)
dated_msg("Creating hourly gridded data")
ptm <- proc.time()
hourly_gridded <- popcycle::group_gridded_by_time(gridded, time_expr = "1 hours", use_data.table = !no_data.table)
data.table::setDTthreads(orig_threads)
gridded <- tibble::as_tibble(gridded)
invisible(gc())
deltat <- proc.time() - ptm
dated_msg("Created hourly gridded data in ", deltat[["elapsed"]], " seconds")

# Create abundance gridded data for hourly data
hourly_gridded <- popcycle::add_abundance(hourly_gridded, hourly_volume, calib=calib)

# Add cruise column
gridded <- gridded %>% dplyr::mutate(cruise = cruise_, .before = 1)
hourly_gridded <- hourly_gridded %>% dplyr::mutate(cruise = cruise_, .before = 1)
if (psd_mode) {
  # PSD only Qc_sum_per_uL, renamed to biomass
  hourly_gridded_PSD <- hourly_gridded %>%
    dplyr::mutate(cruise = cruise_, .before = 1) %>%
    dplyr::select(-c(n, Qc_sum, n_per_uL)) %>%
    dplyr::rename(biomass = Qc_sum_per_uL)
}
dated_msg(
  "Full gridded data, dim = ", stringr::str_flatten(dim(gridded), " "),
  ", size = ", object.size(gridded) / 2**20, " MB"
)
dated_msg(
  "Hourly gridded data, dim = ", stringr::str_flatten(dim(hourly_gridded), " "),
  ", size = ", object.size(hourly_gridded) / 2**20, " MB"
)
if (psd_mode) {
  dated_msg(
    "Hourly gridded data for PSD, dim = ", stringr::str_flatten(dim(hourly_gridded_PSD), " "),
    ", size = ", object.size(hourly_gridded_PSD) / 2**20, " MB"
  )
}

# Save gridded data to file
dated_msg("Writing full gridded data file to ", gridded_out)
ptm <- proc.time()
arrow::write_parquet(gridded, gridded_out)
deltat <- proc.time() - ptm
dated_msg("Wrote full gridded data parquet in ", deltat[["elapsed"]], " seconds")

# Save stat to file
dated_msg("Writing stat to file ", stat_out)
ptm <- proc.time()
arrow::write_parquet(popcycle::get_stat_table(db), stat_out)
deltat <- proc.time() - ptm
dated_msg("Wrote stat parquet in ", deltat[["elapsed"]], " seconds")

# Save volume to file
dated_msg("Writing full volume file to ", volume_out)
ptm <- proc.time()
arrow::write_parquet(volume, volume_out)
deltat <- proc.time() - ptm
dated_msg("Wrote full volume parquet in ", deltat[["elapsed"]], " seconds")

# Save hourly gridded data to file
dated_msg("Writing hourly gridded data file to ", hourly_gridded_out)
ptm <- proc.time()
arrow::write_parquet(hourly_gridded, hourly_gridded_out)
deltat <- proc.time() - ptm
dated_msg("Wrote hourly gridded data parquet in ", deltat[["elapsed"]], " seconds")

# Save hourly gridded data for PSD (Qc_sum_per_uL or biomass) to file
if (psd_mode) {
  dated_msg("Writing hourly gridded data for PSD file to ", hourly_gridded_PSD_out)
  ptm <- proc.time()
  arrow::write_parquet(hourly_gridded_PSD, hourly_gridded_PSD_out)
  deltat <- proc.time() - ptm
  dated_msg("Wrote hourly gridded data for PSD parquet in ", deltat[["elapsed"]], " seconds")
}

# Save hourly volume to file
dated_msg("Writing hourly volume file to ", hourly_volume_out)
ptm <- proc.time()
arrow::write_parquet(hourly_volume, hourly_volume_out)
deltat <- proc.time() - ptm
dated_msg("Wrote hourly volume parquet in ", deltat[["elapsed"]], " seconds")

# # Only keep PAR dates that are in gridded data
# # Average by hour
par <- par %>%
  dplyr::filter(date %in% unique(gridded$date))
hourly_par <- par %>%
  dplyr::group_by(date = lubridate::floor_date(date, "hour")) %>%
  dplyr::summarise(
    par = mean(par, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE)
  )

# # Add cruise column
par <- par %>% dplyr::mutate(cruise = as.factor(cruise_), .before = 1)
hourly_par <- hourly_par %>% dplyr::mutate(cruise = as.factor(cruise_), .before = 1)
# PAR save to file
dated_msg("Writing full PAR file to ", par_out)
arrow::write_parquet(par, par_out)
dated_msg("Writing hourly PAR file to ", hourly_par_out)
arrow::write_parquet(hourly_par, hourly_par_out)

deltat <- proc.time() - t0
message("Script ran in ", lubridate::duration(deltat[["elapsed"]]))
