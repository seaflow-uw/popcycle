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
                        log_base=NULL, log_answers=TRUE) {
  if (length(channel_range) != 2) {
    stop("create_grid: channel_range must be a numeric list of vector of length 2")
  }
  if (length(Qc_range) != 2) {
    stop("create_grid: Qc_range must be a numeric list of vector of length 2")
  }
  grid <- list()
  grid$fsc_small <- create_breaks(bins, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid$pe <- create_breaks(bins, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid$chl_small <- create_breaks(bins, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid$Qc <- create_breaks(bins, Qc_range[[1]], Qc_range[[2]], log_base, log_answers)
  return(grid)
}

create_PSD <- function(db, vct_files, quantile, refracs, grid, log_base=NULL, cores=1,
                       meta=NULL, calib=NULL, use_data.table=TRUE) {
  ptm <- proc.time()

  cores <- min(cores, parallel::detectCores(), length(vct_files))
  cores <- max(cores, 1)

  quantile <- as.numeric(quantile)

  ### Create PSD for each timepoint 
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    counts_list <- tryCatch({
      foreach::`%dopar%`(
        foreach::foreach(
          vct_file=vct_files,
          .inorder=TRUE,
          .packages=c("magrittr"),
          .export=c("create_PSD_one_file")
        ),
        create_PSD_one_file(vct_file, quantile, refracs, grid, log_base=log_base, meta=meta, calib=calib, use_data.table=use_data.table)
      )
    },
    error=function(e) {
      message(e)
      return(NA)
    },
    warnings=function(e) {
      message(w)
      return(NULL)
    },
    finally={
      parallel::stopCluster(cl)
    })
  } else {
    # Serial code
    counts_list <- purrr::map(vct_files, ~ create_PSD_one_file(., quantile, refracs, grid, log_base=log_base, meta=meta, calib=calib, use_data.table=use_data.table))
  }
  counts <- dplyr::bind_rows(counts_list)

  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(counts)
}

create_PSD_one_file <- function(vct_file, quantile, refracs, grid, log_base=NULL,
                                meta=NULL, calib=NULL, use_data.table=TRUE) {
  if (!is.null(calib) && length(unique(calib$cruise)) != 1) {
    stop("calibration table must be limited to one cruise")
  }
  if (nrow(refracs) != 1) {
    stop("refracs should only contain one row")
  }

  quantile <- as.numeric(quantile)
  qstr <- paste0("q", quantile)
  qsuffix <- paste0("_", qstr)

  # Read the VCT parquet file, only grabbing columns needed to analyze one quantile
  vct <- arrow::read_parquet(
    vct_file,
    col_select=c(date, all_of(qstr), fsc_small, pe, chl_small, ends_with(qsuffix))
  )

  # Select one quantile of data
  # Remove quantile boolean column
  # Remove quantile suffix from columns
  vct <- vct %>%
    dplyr::filter(get(qstr)) %>%
    dplyr::select(-c(all_of(qstr))) %>%
    dplyr::rename_with(
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
  for (popname in names(refracs)) {
    refrac_alias <- refracs[, popname]
    pop_idx <- vct$pop == popname
    vct[pop_idx, "Qc"] <- vct[pop_idx, paste0("Qc_", refrac_alias)]
  }

  # Assign each particle to a cell in the grid for each dimension
  for (dim in names(grid)) {
    if (is.null(log_base)) {
      values <- vct[[dim]]
    } else {
      values <- log(vct[[dim]], log_base)
    }

    # Label by index into grid
    vct[paste0(dim, "_coord")] <- as.numeric(cut(values, grid[[dim]], labels=FALSE, include.lowest=TRUE, right=FALSE))

    # Convert index into grid into lower boundary for each bin
    # vct[paste0(dim, "_coord")] <- grid[[dim]][vct[[paste0(dim, "_coord")]]]
  }

  # Group by time, grid coordinates, and population
  # Count cells in each group
  if (use_data.table) {
    # data.table is much faster at this group by than dplyr, sometimes < 1s vs ~30s
    vct <- data.table::as.data.table(vct)
    coord_cols <- stringr::str_subset(names(vct), "_coord$")
    group_cols <- c("date", coord_cols, "pop")
    vct_summary <- vct[, .(n=.N), keyby=group_cols]
    vct_summary <- tibble::as_tibble(vct_summary)
  } else {
    vct_summary <- vct %>% 
      dplyr::group_by(date, dplyr::across(ends_with("_coord")), pop ) %>%
      dplyr::summarise(n=dplyr::n(), .groups="drop")
  }

  # Add metadata
  # Filter by flag
  # Adjust abundance by virtual core size (opp/evt ratio)
  if (!is.null(meta)) {
    vct_summary <- dplyr::left_join(vct_summary, meta, by=c("date" = "time"))
    if ("flag" %in% colnames(meta)) {
        vct_summary <- vct_summary %>% dplyr::filter(flag == 0)
    }
    # TODO: Check for NA, i.e. date in vct_summary but not meta, stop
    vct_summary$volume_adjusted <- vct_summary$volume
    prosyn_idx <- which(vct_summary$pop == "prochloro" | vct_summary$pop == "synecho")
    # Adjust pro/syn volume by per-file opp/evt ratio
    vct_summary[prosyn_idx, "volume_adjusted"] <- vct_summary[prosyn_idx, "volume"] * vct_summary[prosyn_idx, "opp_evt_ratio"]
    # Adjust other populations by median opp/evt ratio for the whole cruise
    vct_summary[-c(prosyn_idx), "volume_adjusted"] <- vct_summary[-c(prosyn_idx), "volume"] * median(meta$opp_evt_ratio)
    # ## calculate cell Abundance (cells uL-1) i.e normalize count by volume of SeaFlow's virtual core.
    vct_summary$n_per_uL <- vct_summary$n / vct_summary$volume_adjusted

    # Correct abundance based on cruise
    if (!is.null(calib)) {
      corrected <- lapply(
        vct_summary %>% dplyr::group_by(date) %>% dplyr::group_split(),
        function(x) {
          x["n_per_uL_calibrated"] <- x["n_per_uL"]
          for (phyto in c("prochloro", "synecho")) {
            corr <- calib %>% dplyr::filter(pop == phyto)
            if (nrow(corr) > 1) {
              stop(paste0("more than one abundance calibration entry found for ", phyto))
            }
            if (nrow(corr) > 0) {
              idx <- which(x$pop == phyto)
              abund <- x[idx, "n_per_uL"]
              if (length(idx) > 0) {
                x[idx, "n_per_uL_calibrated"] <- (calib[["a"]][1] * abund) + (calib[["b"]][1] * (abund / sum(abund)))
              }
            }
          }
          return(x)
        }
      )
      vct_summary <- dplyr::bind_rows(corrected)
    }
  }

  gc()

  return(vct_summary)
}

get_vct_range <- function(vct_dirs, data_col, quantile, cores = 1)  {
  ptm <- proc.time()

  cores <- min(cores, parallel::detectCores())

  # TODO: incorporate outlier files

  # Get vector of file paths
  vct_files <- purrr::flatten_chr(purrr::map(vct_dirs, ~ list.files(., "\\.parquet$", full.names=T)))

  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    answer <- foreach::`%dopar%`(
      foreach::foreach(
        vct_file=vct_files,
        .inorder=TRUE,
        .packages=c("magrittr"),
        .export=c("get_vct_range_one_file")
      ),
      get_vct_range_one_file(vct_file, data_col, quantile)
    )
    answer <- purrr::flatten_dbl(answer)
    parallel::stopCluster(cl)
  } else {
    # Serial code
    answer <- purrr::map(vct_files, ~ get_vct_range_one_file(., data_col, quantile))
    answer <- purrr::flatten_dbl(answer)
  }

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
  meta <- tibble::as_tibble(merge(sfl.all, opp, by="file")[c("time", "lat","lon","volume","opp_evt_ratio","flag")])
  meta$flag <- as.factor(meta$flag)

  return(meta)
}

make_reduced_psd <- function(psd) {
  psd %>%
    dplyr::select(date, fsc_small_coord, pe_coord, chl_small_coord, Qc_coord, pop, n, n_per_uL_calibrated, volume_adjusted) %>%
    dplyr::rename(volume=volume_adjusted, n_per_uL=n_per_uL_calibrated)
}

group_psd_by_time <- function(psd, time_expr="1 hours", use_data.table=TRUE) {
  # data.table is twice as fast for this operation as dplyr in testing on HOT310
  if (use_data.table) {
    psd <- data.table::setDT(psd)
    grouped <- psd[,
        .(n_per_uL = (sum(n_per_uL * volume) / sum(volume)), volume = sum(volume)),
        keyby=.(date=lubridate::floor_date(date, time_expr), fsc_small_coord, pe_coord, chl_small_coord, Qc_coord, pop)
    ]
    grouped <- tibble::as_tibble(grouped)
  } else {
  grouped <- psd %>%
    dplyr::group_by(date=lubridate::floor_date(date, time_expr), fsc_small_coord, pe_coord, chl_small_coord, Qc_coord, pop) %>%
    dplyr::arrange(by_group=TRUE) %>%
    dplyr::summarise(n_per_uL = sum(n_per_uL * volume) / sum(volume), volume = sum(volume), .groups="drop")
  }
  return(grouped)
}

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

# ---------------------------------------------------------------------------- #

parser <- optparse::OptionParser(usage="usage: psd.R [options] db vct_dir")
parser <- optparse::add_option(parser, c("--quantile"), type="character", default="2.5",
  help="Quantile. Choices are 2.5, 50, 97.5. [default %default]",
  metavar="QUANTILE")
parser <- optparse::add_option(parser, c("--out"), type="character", default="PSD",
  help="Output file base path [default %default]",
  metavar="FILE_BASE")
parser <- optparse::add_option(parser, c("--workers"), type="integer", default=1,
  help="Number of worker processes to start [default %default]",
  metavar="N")
parser <- optparse::add_option(parser, c("--bins"), type="integer", default=85,
  help="Number of bins along each dimension of the distribution [default %default]",
  metavar="N")
parser <- optparse::add_option(parser, c("--no-data.table"), action="store_true", default=FALSE,
  help="Don't use data.table for performance-critical aggregation [default %default]")

p <- optparse::parse_args2(parser)
if (length(p$args) < 2) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- normalizePath(p$args[1])
  vct_dir <- normalizePath(p$args[2])
  bins <- p$options$bins
  quantile_ <- p$options$quantile
  cores <- p$options$workers
  out <- p$options$out
  no_data.table <- p$options$no_data.table
}

message(Sys.time())
message("Configuration:")
message("--------------")
message(paste0("db = ", db))
message(paste0("vct-dir = ", vct_dir))
message(paste0("breaks = ", bins))
message(paste0("quantile = ", quantile_))
message(paste0("workers = ", cores))
message(paste0("out = ", out))
message(paste0("no-data.table = ", no_data.table))
message("--------------")
if (!dir.exists(vct_dir) || !file.exists(db)) {
  message(paste0("vct_dir or db does not exist"))
  quit(save=FALSE)
}

cruise <- popcycle::get.cruise(db)
vct_files <- list.files(vct_dir, "\\.parquet$", full.names=T)

meta_full <- create_meta(db, as.numeric(quantile_))
meta_flag <- meta_full[, c("time", "volume", "opp_evt_ratio", "flag")]
meta <- meta_full[, c("time", "volume", "opp_evt_ratio")]

refracs <- popcycle::read_refraction_csv()
refracs <- refracs[refracs$cruise == cruise, ]
refracs$cruise <- NULL

calib_all <- popcycle::read_calib_csv()
calib <- calib_all[calib_all$cruise == cruise, ]

grid <- create_grid(bins, log_base=2, log_answers=FALSE)

psd <- create_PSD(
  db, vct_files, quantile_, refracs, grid, log_base=NULL, cores=cores, 
  meta=meta_flag, calib=calib, use_data.table=!no_data.table
)
dated_msg("Full PSD dim = ", stringr::str_flatten(dim(psd), " "), ", MB = ", object.size(psd) / 2**20)
ptm <- proc.time()
arrow::write_parquet(psd, paste0(out, ".full.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote full parquet in ", deltat[["elapsed"]], " seconds")
# ptm <- proc.time()
# readr::write_csv(psd %>% dplyr::mutate(cruise=cruise) %>% dplyr::rename_with(tolower), paste0(out, ".full.csv.gz"))
# deltat <- proc.time() - ptm
# dated_msg("Wrote full CSV in ", deltat[["elapsed"]], " seconds")

psd_reduced <- make_reduced_psd(psd)
dated_msg("Reduced PSD dim = ", stringr::str_flatten(dim(psd_reduced), " "), ", MB = ", object.size(psd_reduced) / 2**20)
ptm <- proc.time()
arrow::write_parquet(psd_reduced, paste0(out, ".reduced.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote reduced parquet in ", deltat[["elapsed"]], " seconds")
# ptm <- proc.time()
# readr::write_csv(psd_reduced %>% dplyr::mutate(cruise=cruise) %>% dplyr::rename_with(tolower), paste0(out, ".reduced.csv.gz"))
# deltat <- proc.time() - ptm
# dated_msg("Wrote reduced CSV in ", deltat[["elapsed"]], " seconds")

hourly <- group_psd_by_time(psd_reduced, time_expr="1 hours", use_data.table=!no_data.table)
dated_msg("Hourly PSD dim = ", stringr::str_flatten(dim(hourly), " "), ", MB = ", object.size(hourly) / 2**20)
ptm <- proc.time()
arrow::write_parquet(hourly, paste0(out, ".hourly.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly parquet in ", deltat[["elapsed"]], " seconds")
ptm <- proc.time()
readr::write_csv(hourly %>% dplyr::mutate(cruise=cruise) %>% dplyr::rename_with(tolower), paste0(out, ".hourly.csv.gz"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly CSV in ", deltat[["elapsed"]], " seconds")

print(Sys.time())
print("Finished")
