#!/usr/bin/env Rscript

parser <- optparse::OptionParser(
  usage = "usage: psd.R [options] db vct_dir",
  description = "Create gridded particle size distribution data from VCT data"
)
parser <- optparse::add_option(parser, "--bins",
  type = "integer", default = 85, metavar = "N",
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
parser <- optparse::add_option(parser, "--no-data.table",
  action = "store_true", default = FALSE,
  help = "Don't use data.table for performance-critical aggregation [default %default]"
)
parser <- optparse::add_option(parser, "--only-full-counts",
  action = "store_true", default = FALSE,
  help = "Only produce counts for data at full time resolution [default %default]"
)
parser <- optparse::add_option(parser, "--out",
  type = "character", default = "PSD", metavar = "FILE_BASE",
  help = "Output file base path [default %default]",
)
parser <- optparse::add_option(parser, "--quantile",
  type = "character", default = "2.5", metavar = "QUANTILE",
  help = "Quantile. Choices are 2.5, 50, 97.5. [default %default]",
)
parser <- optparse::add_option(parser, "--remove-boundary-points",
  action = "store_true", default = FALSE,
  help = "Remove particles at the per-file boundary of fsc_small, chl_small, pe, diam, Qc [default %default]"
)
parser <- optparse::add_option(parser, "--renv",
  type = "character", default = "", metavar = "dir",
  help = "Optional renv directory to use. Requires the renv package."
)
parser <- optparse::add_option(parser, "--verbose",
  action = "store_true", default = FALSE,
  help = "Print extra diagnostic messages [default %default]"
)
parser <- optparse::add_option(parser, "--volume",
  type = "integer", default = NULL, metavar = "VOLUME",
  help = "Hard code a single volume value for abundance calculations, i.e. if stream pressure or file duration is unreliable [default %default]",
)

p <- optparse::parse_args2(parser)
if (length(p$args) < 2) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- normalizePath(p$args[1])
  vct_dir <- normalizePath(p$args[2])
  bins <- p$options$bins
  dimensions <- p$options$dimensions
  keep_outliers <- p$options$keep_outliers
  no_data.table <- p$options$no_data.table
  only_full_counts <- p$options$only_full_counts
  out <- p$options$out
  quantile_ <- p$options$quantile
  remove_boundary_points <- p$options$remove_boundary_points
  verbose <- p$options$verbose
  volume <- p$options$volume

  if (!dir.exists(vct_dir) || !file.exists(db)) {
    stop(paste0("vct_dir or db does not exist"))
  }

  if (p$options$renv != "") {
    proj_dir <- renv::activate(p$options$renv)
    message("activated renv directory ", proj_dir)
  }
}

message("using popcycle version ", packageVersion("popcycle"))

library(dplyr, warn.conflicts=FALSE)

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

# --------------------------
# Set up dimensions for grid
# --------------------------
possible_dimensions <- unlist(stringr::str_split("fsc_small,pe,chl_small,Qc,diam", "\\s*,\\s*"))
dimensions <- unlist(stringr::str_split(dimensions, "\\s*,\\s*"))
if (length(setdiff(dimensions, possible_dimensions)) > 0) {
  stop(glue("Error: unknown dimensions {stringr::str_flatten(setdiff(dimensions, possible), ', ')}"))
}
# Always add Qc
if (! "Qc" %in% dimensions) {
  dimensions[length(dimensions) + 1] <- "Qc"
}

dated_msg("Start")
message("Configuration:")
message("--------------")
message(paste0("db = ", db))
message(paste0("vct-dir = ", vct_dir))
message(paste0("bins = ", bins))
message(paste0("dimensions = ", stringr::str_flatten(dimensions, ", ")))
message(paste0("keep_outliers = ", keep_outliers))
message(paste0("no-data.table = ", no_data.table))
message(paste0("out = ", out))
message(paste0("quantile = ", quantile_))
message(paste0("remove-boundary-points = ", remove_boundary_points))
message(paste0("verbose = ", verbose))
message(paste0("volume = ", volume))

message("--------------")

# Create output directory tree
dir.create(dirname(out), recursive=T, showWarnings=F)

cruise <- popcycle::get_cruise(db)
vct_files <- list.files(vct_dir, "\\.parquet$", full.names=T)

meta_full <- popcycle::create_meta(db, as.numeric(quantile_))
meta <- meta_full[, c("date", "volume", "opp_evt_ratio", "flag")]
if (! is.null(volume)) {
  dated_msg(glue::glue("setting volume to {volume} in meta table"))
  meta$volume <- volume
}

dated_msg("Retrieving refractive index table")
refracs <- popcycle::read_refraction_csv()
refracs <- refracs[refracs$cruise == cruise, ]
dated_msg(paste0(capture.output(refracs), collapse="\n"))
refracs$cruise <- NULL
if (nrow(refracs) == 0) {
  dated_msg(paste0("refractive index table has no entry for ", cruise, ", using mid for Pro/Syn, lwr for Pico/Croco/Beads/Unknown"))
  refracs <- tibble::as_tibble(data.frame(
    prochloro = "mid", synecho = "mid", croco = "lwr", picoeuk = "lwr", beads = "lwr", unknown = "lwr"
  ))
}
dated_msg(paste0(capture.output(refracs), collapse="\n"))
if (nrow(refracs) > 1) {
  stop(paste0("refractive index table has multiple entries for ", cruise))
}

dated_msg("Retrieving influx calibration table")
calib_all <- popcycle::read_calib_csv()
calib <- calib_all[calib_all$cruise == cruise, ]
if (length(unique(calib$cruise)) == 0) {
  dated_msg(paste0("calibration table has no entry for ", cruise, ", no calibration will be applied"))
  calib <- NULL
}
dated_msg(paste0(capture.output(calib), collapse="\n"))

# Make the grid
grid <- popcycle::create_grid(bins, log_base=2, log_answers=FALSE)
grid_df <- tibble::tibble(fsc_small=grid$fsc_small, pe=grid$pe, chl_small=grid$chl_small, Qc=grid$Qc, diam=grid$diam)
# Subset down to dimensions from CLI
grid <- grid[dimensions]
grid_df <- grid_df[dimensions]
arrow::write_parquet(grid_df, paste0(out, ".grid.parquet"))

# ---------------------------------
# Create the full gridded data file
# ---------------------------------
if (!keep_outliers) {
  ignore_dates <- meta %>% filter(flag != 0) %>% pull(date)
} else {
  ignore_dates <- NULL
}
psd <- popcycle::create_PSD(
  vct_files, quantile_, refracs, grid, log_base=NULL,
  remove_boundary_points=remove_boundary_points, ignore_dates=ignore_dates,
  use_data.table=!no_data.table, verbose=verbose
)
invisible(gc())
dated_msg("Full PSD dim = ", stringr::str_flatten(dim(psd), " "), ", MB = ", object.size(psd) / 2**20)
ptm <- proc.time()
arrow::write_parquet(psd, paste0(out, ".full.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote full parquet in ", deltat[["elapsed"]], " seconds")
invisible(gc())

if (!only_full_counts) {
  # -----------
  # Hourly file
  # -----------
  # data.table multi-threading temporarily disabled for grouping
  orig_threads <- data.table::getDTthreads()
  data.table::setDTthreads(1)
  hourly <- popcycle::group_psd_by_time(psd, time_expr="1 hours", use_data.table=!no_data.table)
  data.table::setDTthreads(orig_threads)
  psd <- tibble::as_tibble(psd)
  invisible(gc())

  # Add volume-normalized abundances to hourly data
  if (!keep_outliers) {
    meta <- meta %>% dplyr::filter(flag == 0)  # remove flagged files
  }
  hourly_volumes <- popcycle::create_volume_table(meta, time_expr="1 hour")
  hourly_psd <- popcycle::add_adundance(hourly, hourly_volumes, calib=calib)

  dated_msg("Hourly PSD dim = ", stringr::str_flatten(dim(hourly_psd), " "), ", MB = ", object.size(hourly_psd) / 2**20)
  ptm <- proc.time()
  arrow::write_parquet(hourly_psd, paste0(out, ".hourly.parquet"))
  deltat <- proc.time() - ptm
  dated_msg("Wrote hourly parquet in ", deltat[["elapsed"]], " seconds")

  ptm <- proc.time()
  readr::write_csv(hourly_psd %>% dplyr::mutate(cruise=cruise) %>% dplyr::rename_with(tolower), paste0(out, ".hourly.csv.gz"))
  deltat <- proc.time() - ptm
  dated_msg("Wrote hourly CSV in ", deltat[["elapsed"]], " seconds")

  ptm <- proc.time()
  arrow::write_parquet(hourly_volumes %>% dplyr::select(date, volume_small, volume_large), paste0(out, ".hourly-volumes.parquet"))
  deltat <- proc.time() - ptm
  dated_msg("Wrote hourly volume parquet in ", deltat[["elapsed"]], " seconds")
}

dated_msg("Finished")
