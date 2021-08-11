#!/usr/bin/env Rscript
library(magrittr)

create_breaks <- function(bins, minval, maxval, log_base=NULL, log_answers=TRUE) {
  if (!is.null(log_base)) {
    minval <- log(minval, base=log_base)
    maxval <- log(maxval, base=log_base)
  }
  b <- seq(from=minval, to=maxval, length=bins+1)
  if (!is.null(log_base) && !log_answers) {
    return(log_base^(b))
  }
  return(b)
}

create_grid <- function(bins=85, channel_range=c(1, 3200), Qc_range=c(0.002, 1600),
                        diam_range=c(0.1, 37), log_base=NULL, log_answers=TRUE) {
  if (length(channel_range) != 2) {
    stop("create_grid: channel_range must be a two-item numeric list or vector")
  }
  if (length(Qc_range) != 2) {
    stop("create_grid: Qc_range must be a two-item numeric list or vector")
  }
  if (length(diam_range) != 2) {
    stop("create_grid: diam_range must be a two-item numeric list or vector")
  }
  grid <- list()
  grid$fsc_small <- create_breaks(bins, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid$pe <- create_breaks(bins, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid$chl_small <- create_breaks(bins, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid$Qc <- create_breaks(bins, Qc_range[[1]], Qc_range[[2]], log_base, log_answers)
  grid$diam <- create_breaks(bins, diam_range[[1]], diam_range[[2]], log_base, log_answers)
  return(grid)
}

create_PSD <- function(db, vct_files, quantile, refracs, grid, log_base=NULL, remove_boundary_points=FALSE,
                       use_data.table=TRUE, verbose=FALSE) {
  ptm <- proc.time()
  quantile <- as.numeric(quantile)
  counts_list <- purrr::map(vct_files, ~ create_PSD_one_file(., quantile, refracs, grid, log_base=log_base,
                                                             remove_boundary_points=remove_boundary_points,
                                                             use_data.table=use_data.table, verbose=verbose))
  counts <- dplyr::bind_rows(counts_list)
  rm(counts_list)
  invisible(gc())
  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(counts)
}

create_PSD_one_file <- function(vct_file, quantile, refracs, grid, log_base=NULL, remove_boundary_points=FALSE,
                                use_data.table=TRUE, verbose=FALSE) {
  diag_text <- list(vct_file)  # for verbose diagnostics
  if (nrow(refracs) != 1) {
    stop("refracs should only contain one row")
  }

  qstr <- paste0("q", quantile)
  qsuffix <- paste0("_", qstr)

  # Read the VCT parquet file, only grabbing columns needed to analyze one quantile
  vct <- arrow::read_parquet(
    vct_file,
    col_select=c(date, all_of(qstr), fsc_small, pe, chl_small, ends_with(qsuffix))
  )

  vct <- vct %>%
    dplyr::filter(get(qstr)) %>%         # select one quantile of data
    dplyr::select(-c(all_of(qstr))) %>%  # remove quantile boolean column
    dplyr::rename_with(                  # Remove quantile suffix from columns
      function(x) { stringr::str_replace(x, paste0(qsuffix, "$"), "") },
      ends_with(qsuffix)
    )

  # Create new "diam" and "Qc" columns with the correct refractive index for
  # each population
  # For unknown ands beads, use the same value as picoeuk
  if ("picoeuk" %in% names(refracs)) {
    refracs$unknown <- refracs$picoeuk
    refracs$beads <- refracs$picoeuk
  } else {
    stop("missing picoeuk defintition from refracs")
  }
  refrac_status <- list("  refractive indexes used:")  # verbose output for refractive indices
  for (popname in names(refracs)) {
    refrac_alias <- refracs[[1, popname]]
    # Track which refractive index we used for this pop
    refrac_status[[length(refrac_status)+1]] <- paste0(popname, "=", refrac_alias)
    pop_idx <- vct$pop == popname
    vct[pop_idx, "Qc"] <- vct[pop_idx, paste0("Qc_", refrac_alias)]
    vct[pop_idx, "diam"] <- vct[pop_idx, paste0("diam_", refrac_alias)]
  }
  diag_text[[length(diag_text)+1]] <- paste(refrac_status, collapse=" ")

  # Remove particles at the max value for any of the three basic detectors
  if (remove_boundary_points) {
    max_prop <- 0.2  # max proportion of boundary points before error
    orig_len <- nrow(vct)
    vct <- vct %>%
      dplyr::filter(
        fsc_small > min(fsc_small),
        fsc_small < max(fsc_small),
        chl_small > min(chl_small),
        chl_small < max(chl_small),
        pe > min(pe),
        pe < max(pe),
        diam > min(diam),
        diam < max(diam),
        Qc > min(Qc),
        Qc < max(Qc)
      )
    if(orig_len - nrow(vct) > max_prop * orig_len) {
      stop(paste0("create_PSD_one_file: too many boundary points removed in ", vct_file))
    }
  }

  # Assign each particle to a cell in the grid for each dimension
  for (dim in names(grid)) {
    if (is.null(log_base)) {
      values <- vct[[dim]]
    } else {
      values <- log(vct[[dim]], log_base)
    }

    # Label by index into grid
    vct[paste0(dim, "_coord")] <- as.integer(cut(values, grid[[dim]], labels=FALSE, right=FALSE))
    if (any(is.na(vct[paste0(dim, "_coord")]))) {
      stop(paste0("create_PSD_one_file: ", dim, " value out of range in ", vct_file))
    }

    # Convert index into grid into lower boundary for each bin
    # vct[paste0(dim, "_coord")] <- grid[[dim]][vct[[paste0(dim, "_coord")]]]
  }

  # Group by time, grid coordinates, and population
  # Count cells in each group
  if (use_data.table) {
    # data.table is much faster at this group by than dplyr, sometimes < 1s vs ~30s
    data.table::setDTthreads(1)  # turn off data.table multi-threading
    vct <- data.table::as.data.table(vct)
    coord_cols <- stringr::str_subset(names(vct), "_coord$")
    group_cols <- c("date", coord_cols, "pop")
    vct_summary <- vct[, .(n=.N, Qc_sum=sum(Qc)), keyby=group_cols]
    vct_summary <- tibble::as_tibble(vct_summary)
  } else {
    vct_summary <- vct %>% 
      dplyr::group_by(date, dplyr::across(ends_with("_coord")), pop ) %>%
      dplyr::summarise(n=dplyr::n(), Qc_sum=sum(Qc), .groups="drop")
  }

  if (verbose) {
    message(paste(diag_text, collapse="\n"))
  }

  return(vct_summary)
}

get_vct_range <- function(vct_dirs, data_col, quantile)  {
  ptm <- proc.time()
  # Get vector of file paths
  vct_files <- purrr::flatten_chr(purrr::map(vct_dirs, ~ list.files(., "\\.parquet$", full.names=T)))
  answer <- purrr::map(vct_files, ~ get_vct_range_one_file(., data_col, quantile))
  answer <- purrr::flatten_dbl(answer)
  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(c(min(answer), max(answer)))
}

get_vct_range_one_file <- function(vct_file, data_col, quantile) {
  # Check data column requested for validity and for presence of multiple refractive indexes
  refractive_cols <- c("Qc", "diam")
  channel_cols <- c("fsc_small", "chl_small", "pe")
  
  if (! (data_col %in% c(refractive_cols, channel_cols))) {
    stop(paste("data_col must be one of", paste(channel_cols, collapse=" "), paste(refractive_cols, collapse=" ")))
  }
  refractive <- data_col %in% refractive_cols

  qstr <- paste0("q", as.numeric(quantile))
  qsuffix <- paste0("_", qstr)

  # Get date, quantile boolean column, data columns, and pop columns from file
  if (refractive) {
    vct <- arrow::read_parquet(
      vct_file,
      col_select=c(all_of(qstr), c(starts_with(data_col) & ends_with(qsuffix)))
    )
  } else {
    vct <- arrow::read_parquet(vct_file, col_select=c(all_of(qstr), all_of(data_col)))
  }
  # Find min and max for each data column
  minmax <- vct %>%
    dplyr::filter(get(qstr)) %>%
    dplyr::summarise(dplyr::across(starts_with(data_col), ~ range(.x)))
  return(c(min(as.matrix(minmax)), max(as.matrix(minmax))))
}

create_meta <- function(db, quantile) {
  quantile <- as.numeric(quantile)

  ### Retrieve metadata
  ## Retrieve SFL table
  sfl <- popcycle::get.sfl.table(db)
  # format time
  sfl$time <- as.POSIXct(sfl$date, format="%FT%T", tz="UTC")
  # retrieve flow rate (mL min-1) of detectable volume
  fr <- popcycle::flowrate(sfl$stream_pressure, inst=popcycle::get.inst(db))$flow_rate
  # convert to microL min-1
  fr <- fr * 1000
  # acquisition time (min)
  acq.time <- sfl$file_duration/60
  # volume in microL
  sfl$volume <- round(fr * acq.time , 0)

  ## Retrive Outlier table
  outliers <- popcycle::get.outlier.table(db)
  # merge with sfl
  sfl.all <- merge(sfl, outliers, by="file")

  ## Retrive OPP table
  # retrieve opp/evt
  opp <- tibble::as_tibble(popcycle::get.opp.table(db))
  opp <- opp[opp$quantile == quantile, ]

  ## merge all metadata
  meta <- tibble::as_tibble(merge(sfl.all, opp, by="file")[c("time", "volume", "opp_evt_ratio", "flag")])
  meta$flag <- as.factor(meta$flag)

  return(meta)
}

group_psd_by_time <- function(psd, time_expr="1 hours", use_data.table=TRUE) {
  # data.table is twice as fast for this operation as dplyr in testing on HOT310
  if (use_data.table) {
    # This is a side-effect, converting psd by reference to a data.table, and
    # as such this effect will persist after the functions exits. Not great, but
    # this object can very large and I'd rather not make a copy.
    psd <- data.table::setDT(psd)
    grouped <- psd[,
                   .(n=sum(n), Qc_sum=sum(Qc_sum)),
                   keyby=.(date=lubridate::floor_date(date, time_expr), fsc_small_coord, pe_coord, chl_small_coord, Qc_coord, diam_coord, pop)
                   ]
    grouped <- tibble::as_tibble(grouped)
  } else {
    grouped <- psd %>%
      dplyr::group_by(date=lubridate::floor_date(date, time_expr), fsc_small_coord, pe_coord, chl_small_coord, Qc_coord, diam_coord, pop) %>%
      dplyr::arrange(by_group=TRUE) %>%
      dplyr::summarise(n=sum(n), Qc_sum=sum(Qc_sum), .groups="drop")
  }
  return(grouped)
}

create_volume_table <- function(meta, remove_flagged=FALSE, group_by_time=FALSE, time_expr="1 hour") {
  meta <- meta %>% dplyr::select(time, volume, opp_evt_ratio, flag)
  if (remove_flagged) {
    meta <- meta %>% dplyr::filter(flag == 0)
  }
  meta <- meta %>%
    dplyr::mutate(
      volume_file = volume * opp_evt_ratio,
      volume_global = volume * median(opp_evt_ratio)
    )
  if (group_by_time) {
    meta <- meta %>%
      dplyr::mutate(time=lubridate::floor_date(time, time_expr)) %>%
      dplyr::group_by(time) %>%
      dplyr::arrange(by_group=TRUE) %>%
      dplyr::summarise(
        volume=sum(volume),
        volume_file=sum(volume_file),
        volume_global=sum(volume_global)
      )
  }

  # Just to be consistent with the rest of the datetime column names
  meta <- meta %>% dplyr::rename(date = time)

  return(meta)
}

add_adundance <- function(psd, volumes, calib=NULL) {
  # Calibrate to influx data if provided
  if (!is.null(calib)) {
    if (length(unique(calib$cruise)) == 0) {
      stop("calibration table is empty")
    }
    psd$n <- as.double(psd$n) # to ensure a consistent column type
    corrected <- lapply(
      psd %>% dplyr::group_by(date) %>% dplyr::group_split(),
      function(x) {
        for (phyto in c("prochloro", "synecho")) {
          corr <- calib %>% dplyr::filter(pop == phyto)
          if (nrow(corr) > 1) {
            stop(paste0("more than one abundance calibration entry found for ", phyto))
          }
          a <- calib[["a"]][1]
          b <- calib[["b"]][1]
          pop_idx <- x$pop == phyto
          n_vals <- x[pop_idx, "n"]
          Qc_sum_vals <- x[pop_idx, "Qc_sum"]
          x[pop_idx, "n"] <- a * n_vals + (b * (n_vals / sum(n_vals)))
          x[pop_idx, "Qc_sum"] <- a * Qc_sum_vals + (b * (Qc_sum_vals / sum(Qc_sum_vals)))
        }
        return(x)
      }
    )
    psd <- dplyr::bind_rows(corrected)
  }

  # Calculate abundance
  psd <- dplyr::left_join(psd, volumes, by="date")
  psd$n_per_uL <- psd$n / psd$volume_global
  psd$Qc_sum_per_uL <- psd$Qc_sum / psd$volume_global
  pop_idx <- psd$pop == "prochloro" | psd$pop == "synecho"
  psd[pop_idx, "n_per_uL"] <- psd[pop_idx, "n"] / psd[pop_idx, "volume_file"]
  psd[pop_idx, "Qc_sum_per_uL"] <- psd[pop_idx, "Qc_sum"] / psd[pop_idx, "volume_file"]
  psd <- psd %>% dplyr::select(-c(volume, volume_file, volume_global))

  return(psd)
}

validate_psd <- function(psd, vct, grid, refrac) {
  qc_col <- paste0("Qc_", refrac)
  for (i in seq_along(psd$date)) {
    psdr <- psd[i, ]
    vct_match <- vct %>%
      dplyr::filter(
        date == psdr$date,
        fsc_small >= grid$fsc_small[psdr$fsc_small_coord],
        fsc_small < grid$fsc_small[psdr$fsc_small_coord + 1],
        pe >= grid$pe[psdr$pe_coord],
        pe < grid$pe[psdr$pe_coord + 1],
        chl_small >= grid$chl_small[psdr$chl_small_coord],
        chl_small < grid$chl_small[psdr$chl_small_coord + 1],
        !!dplyr::sym(qc_col) >= grid$Qc[psdr$Qc_coord],
        !!dplyr::sym(qc_col) < grid$Qc[psdr$Qc_coord + 1],
        pop == psdr$pop
      )
    msg <- paste(c(paste0("psd=", psdr$n), paste0("vct=", nrow(vct_match)), as.character(psdr$date), psdr$fsc_small_coord, psdr$pe_coord, psdr$chl_small_coord, psdr$Qc_coord, as.character(psdr$pop), psdr$n), collapse=" ")
    if (psdr$n != nrow(vct_match)) {
      msg <- paste0("MISMATCH: ", msg)
    }
    print(msg)
  }
}

grid_labels <- function(grid) {
  labels <- list()
  for (channel in names(grid)) {
    g <- grid[[channel]]
    labels[[channel]] <- sapply(seq(1, length(g)-2), function(i) { paste0("[", format(round(g[i], 4), nsmall = 4), "-", format(round(g[i+1], 4), nsmall = 4), ")") })
    i <- length(g) - 1
    labels[[channel]] <- c(labels[[channel]], paste0("[", format(round(g[i], 4), nsmall = 4), "-", format(round(g[i+1], 4), nsmall = 4), "]"))
  }
  
  return(labels)
}

add_coord_labels <- function(df, grid) {
  labels <- grid_labels(grid)
  for (col in colnames(df)) {
    if (endsWith(col, "_coord")) {
      colbase <- stringr::str_replace(col, "_coord$", "")
      newcol <- paste0(colbase, "_label")
      df <- df %>%
        dplyr::add_column("{newcol}" := labels[[colbase]][df[[col]]], .after = col)
    }
  }
  return(df)
}

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

# ---------------------------------------------------------------------------- #

parser <- optparse::OptionParser(usage="usage: psd.R [options] db vct_dir")
parser <- optparse::add_option(parser, c("--bins"), type="integer", default=85,
  help="Number of bins along each dimension of the distribution [default %default]",
  metavar="N")
parser <- optparse::add_option(parser, c("--no-data.table"), action="store_true", default=FALSE,
  help="Don't use data.table for performance-critical aggregation [default %default]")
parser <- optparse::add_option(parser, c("--out"), type="character", default="PSD",
  help="Output file base path [default %default]",
  metavar="FILE_BASE")
parser <- optparse::add_option(parser, c("--quantile"), type="character", default="2.5",
  help="Quantile. Choices are 2.5, 50, 97.5. [default %default]",
  metavar="QUANTILE")
parser <- optparse::add_option(parser, c("--remove-boundary-points"), action="store_true", default=FALSE,
  help="Remove particles at the per-file boundary of fsc_small, chl_small, pe, diam, Qc [default %default]")
parser <- optparse::add_option(parser, c("--verbose"), action="store_true", default=FALSE,
  help="Print extra diagnostic messages [default %default]")

p <- optparse::parse_args2(parser)
if (length(p$args) < 2) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- normalizePath(p$args[1])
  vct_dir <- normalizePath(p$args[2])
  bins <- p$options$bins
  no_data.table <- p$options$no_data.table
  out <- p$options$out
  quantile_ <- p$options$quantile
  remove_boundary_points <- p$options$remove_boundary_points
  verbose <- p$options$verbose
}

dated_msg("Start")
message("Configuration:")
message("--------------")
message(paste0("db = ", db))
message(paste0("vct-dir = ", vct_dir))
message(paste0("bins = ", bins))
message(paste0("no-data.table = ", no_data.table))
message(paste0("out = ", out))
message(paste0("quantile = ", quantile_))
message(paste0("remove-boundary-points = ", remove_boundary_points))
message(paste0("verbose = ", verbose))

message("--------------")
if (!dir.exists(vct_dir) || !file.exists(db)) {
  message(paste0("vct_dir or db does not exist"))
  quit(save=FALSE)
}
# Create output directory tree
dir.create(dirname(out), recursive=T, showWarnings=F)

cruise <- popcycle::get.cruise(db)
vct_files <- list.files(vct_dir, "\\.parquet$", full.names=T)

meta_full <- create_meta(db, as.numeric(quantile_))
meta <- meta_full[, c("time", "volume", "opp_evt_ratio", "flag")]

dated_msg("Retrieving refractive index table")
refracs <- popcycle::read_refraction_csv()
refracs <- refracs[refracs$cruise == cruise, ]
dated_msg(paste0(capture.output(refracs), collapse="\n"))
refracs$cruise <- NULL
if (nrow(refracs) == 0) {
  dated_msg(paste0("refractive index table has no entry for ", cruise, ", using mid for Pro/Syn, lwr for Pico/Croco"))
  refracs <- tibble::as_tibble(data.frame(prochloro="mid", synecho="mid", croco="lwr", picoeuk="lwr"))
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

grid <- create_grid(bins, log_base=2, log_answers=FALSE)
grid_df <- tibble::tibble(fsc_small=grid$fsc_small, pe=grid$pe, chl_small=grid$chl_small, Qc=grid$Qc, diam=grid$diam)
arrow::write_parquet(grid_df, paste0(out, ".grid.parquet"))

# ---------------------------------
# Create the full gridded data file
# ---------------------------------
psd <- create_PSD(
  db, vct_files, quantile_, refracs, grid, log_base=NULL,
  remove_boundary_points=remove_boundary_points, use_data.table=!no_data.table,
  verbose=verbose
)
# Add flags
psd <- dplyr::left_join(psd, meta %>% dplyr::select(time, flag), by=c("date" = "time"))
invisible(gc())
dated_msg("Full PSD dim = ", stringr::str_flatten(dim(psd), " "), ", MB = ", object.size(psd) / 2**20)
ptm <- proc.time()
arrow::write_parquet(psd, paste0(out, ".full.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote full parquet in ", deltat[["elapsed"]], " seconds")
invisible(gc())

# -----------
# Hourly file
# -----------
# Remove flagged files
psd <- psd %>% dplyr::filter(flag == 0)
# data.table multi-threading temporarily enabled for grouping
orig_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)
hourly <- group_psd_by_time(psd, time_expr="1 hours", use_data.table=!no_data.table)
data.table::setDTthreads(orig_threads)
psd <- tibble::as_tibble(psd)
invisible(gc())

# Add volume-normalized abundances to hourly data
volumes <- create_volume_table(meta, remove_flagged=TRUE, group_by_time=TRUE)
hourly <- add_adundance(hourly, volumes, calib=calib)

dated_msg("Hourly PSD dim = ", stringr::str_flatten(dim(hourly), " "), ", MB = ", object.size(hourly) / 2**20)
ptm <- proc.time()
arrow::write_parquet(hourly, paste0(out, ".hourly.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly parquet in ", deltat[["elapsed"]], " seconds")

ptm <- proc.time()
readr::write_csv(hourly %>% dplyr::mutate(cruise=cruise) %>% dplyr::rename_with(tolower), paste0(out, ".hourly.csv.gz"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly CSV in ", deltat[["elapsed"]], " seconds")

ptm <- proc.time()
arrow::write_parquet(volumes %>% dplyr::select(date, volume_file, volume_global), paste0(out, ".hourly-volumes.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly volume parquet in ", deltat[["elapsed"]], " seconds")

dated_msg("Finished")
