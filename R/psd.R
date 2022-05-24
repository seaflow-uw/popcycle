#' Create particle size distributions as particle counts and Qc sums for many VCT files
#'
#' @param vct_files VCT files to grid.
#' @param quantile OPP filtering quantile.
#' @param refracs Dataframe of refractive indices to use for each population. Column names
#'   should be population names used in classification, values should be mid, lwr, upr.
#' @param grid Named list of break points to use for gridding. Names should be fsc_small, pe,
#'   chl_small, Qc, diam.
#' @param log_base If the break points are logged values, provide the base here to properly
#'   log the VCT data before gridding. If break points are not log use NULL.
#' @param remove_boundary_points Remove min and max particles by fsc_small, pe, chl_small, Qc,
#'   diam before gridding.
#' @param ignore_dates Don't process VCT data with these dates.
#' @param pop A single populations label to keep. Particles not matching this label will be
#'   ignored.
#' @param use_data.table Use data.table for performance speedup, otherwise use dplyr.
#' @param verbose Message extra diagnostic information.
#' @param cores Number of cores to use
#' @return Tibble of gridded data, grouped by date, fsc_small, pe, chl_small, Qc diam grid
#'   locations, and population. Each grid coordinate is an integer index into the corresponding
#'   grid breaks vector. The break point at that location defines the inclusive lower bound
#'   of the bin, and the next break point in the sequence defines the exclusive upper bound.
#'   Data columns summarizing each group are n for particle count and Qc_sum for the the sum
#'   of Qc.
#' @export
create_PSD <- function(vct_files, quantile, refracs, grid, log_base=NULL, remove_boundary_points=FALSE,
                       ignore_dates=NULL, pop=NULL, use_data.table=TRUE, verbose=FALSE, cores = 1) {
  ptm <- proc.time()
  quantile <- as.numeric(quantile)
  cores <- min(cores, parallel::detectCores())
  message("using ", cores, " cores")
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    counts_list <- foreach::foreach(v = vct_files, .inorder = TRUE) %dopar% {
      create_PSD_one_file(v, quantile, refracs, grid, log_base=log_base,
                          remove_boundary_points=remove_boundary_points,
                          use_data.table=use_data.table, ignore_dates=ignore_dates,
                          pop=pop, verbose=verbose)
    }
    parallel::stopCluster(cl)
  } else {
    # Serial code
    counts_list <- lapply(vct_files, function(v) {
      create_PSD_one_file(v, quantile, refracs, grid, log_base=log_base,
                          remove_boundary_points=remove_boundary_points,
                          use_data.table=use_data.table, ignore_dates=ignore_dates,
                          pop=pop, verbose=verbose)
    })
  }
  counts <- dplyr::bind_rows(counts_list)
  rm(counts_list)
  invisible(gc())
  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(counts)
}

#' Create particle size distribution as particle counts and Qc sums for one VCT file
#'
#' @param vct_file VCT file path.
#' @param quantile OPP filtering quantile.
#' @param refracs Dataframe of refractive indices to use for each population. Column names
#'   should be population names used in classification, values should be mid, lwr, upr.
#' @param grid Named list of break points to use for gridding. Names should be fsc_small, pe,
#'   chl_small, Qc, diam.
#' @param log_base If the break points are logged values, provide the base here to properly
#'   log the VCT data before gridding. If break points are not log use NULL.
#' @param remove_boundary_points Remove min and max particles by fsc_small, pe, chl_small, Qc,
#'   diam before gridding.
#' @param ignore_dates Don't process VCT data with these dates.
#' @param pop A single population label to keep. Particles not matching this label will be
#'   ignored.
#' @param use_data.table Use data.table for performance speedup, otherwise use dplyr.
#' @param verbose Message extra diagnostic information.
#' @return Tibble of gridded data, grouped by date, fsc_small, pe, chl_small, Qc diam grid
#'   locations, and population. Each grid coordinate is an integer index into the corresponding
#'   grid breaks vector. The break point at that location defines the inclusive lower bound
#'   of the bin, and the next break point in the sequence defines the exclusive upper bound.
#'   Data columns summarizing each group are n for particle count and Qc_sum for the the sum
#'   of Qc.
create_PSD_one_file <- function(vct_file, quantile, refracs, grid, log_base = NULL, remove_boundary_points = FALSE,
                                ignore_dates = NULL, pop = NULL, use_data.table = TRUE, verbose = FALSE) {
  diag_text <- list(vct_file)  # for verbose diagnostics
  if (nrow(refracs) != 1) {
    stop("refracs should only contain one row")
  }
  if (!all(refracs[1, ] %in% c("mid", "lwr", "upr"))) {
    stop("invalid refraction index label in: '", paste(refracs, collapse = " "), "'")
  }

  qstr <- paste0("q", quantile)
  qsuffix <- paste0("_", qstr)

   # Read the VCT parquet file, only grabbing columns needed to analyze one quantile
  # This is a lot of code to read only columns needed, but I feel it's clearer
  # than trying to dynamically build a composed tidyselect expression for col_select
  cols_needed <- c("date", qstr, paste0("pop", qsuffix))
  for (col in names(grid)) {
    if (!(col %in% c("diam", "Qc"))) {
      cols_needed <- c(cols_needed, col)
    }
  }
  need_diam <- "diam" %in% names(grid)
  need_Qc <- "Qc" %in% names(grid)
  if (!need_diam && !need_Qc) {
    vct <- arrow::read_parquet(
        vct_file,
        col_select = c(all_of(cols_needed))
    )
  } else if (need_diam && !need_Qc) {
    vct <- arrow::read_parquet(
        vct_file,
        col_select = c(all_of(cols_needed), (starts_with("diam") & ends_with(qsuffix)))
    )
  } else if (!need_diam && need_Qc) {
    vct <- arrow::read_parquet(
        vct_file,
        col_select = c(all_of(cols_needed), (starts_with("Qc") & ends_with(qsuffix)))
    )
  } else {
    vct <- arrow::read_parquet(
        vct_file,
        col_select = c(all_of(cols_needed), ends_with(qsuffix))
    )
  }
  vct <- vct %>%
    dplyr::filter(get(qstr)) %>%         # select one quantile of data
    dplyr::select(-c(all_of(qstr))) %>%  # remove quantile boolean column
    dplyr::rename_with(                  # Remove quantile suffix from columns
      function(x) { stringr::str_replace(x, paste0(qsuffix, "$"), "") },
      ends_with(qsuffix)
    )
  if (!is.null(pop)) {
    vct <- vct %>% dplyr::filter(pop == {{ pop }})
  }

  # Ignore certain dates
  if (!is.null(ignore_dates)) {
    vct <- vct %>% dplyr::filter(! (date %in% ignore_dates))
  }

  # Create new "diam" and "Qc" columns with the correct refractive index for
  # each population
  refrac_status <- list("  refractive indexes used:")  # verbose output for refractive indices
  for (popname in names(refracs)) {
    refrac_alias <- refracs[[1, popname]]
    # Track which refractive index we used for this pop
    refrac_status[[length(refrac_status)+1]] <- paste0(popname, "=", refrac_alias)
    pop_idx <- vct$pop == popname
    if ("Qc" %in% names(grid)) {
      vct[pop_idx, "Qc"] <- vct[pop_idx, paste0("Qc_", refrac_alias)]
    }
    if ("diam" %in% names(grid)) {
      vct[pop_idx, "diam"] <- vct[pop_idx, paste0("diam_", refrac_alias)]
    }
  }
  if (("Qc" %in% names(grid)) && any(is.na(vct$Qc))) {
    stop(paste0("create_PSD_one_file: missing refractive index for at least one population in ", vct_file))
  }
  if (("diam" %in% names(grid)) && any(is.na(vct$diam))) {
    stop(paste0("create_PSD_one_file: missing refractive index for at least one population in ", vct_file))
  }
  diag_text[[length(diag_text)+1]] <- paste(refrac_status, collapse=" ")

  # Remove particles at the max value for any dimension
  if (remove_boundary_points && nrow(vct) > 0) {
    max_prop <- 0.2  # max proportion of boundary points before error
    orig_len <- nrow(vct)
    vct <- vct %>%
      dplyr::group_by(date) %>%
      dplyr::group_modify(function(x, y) {
        sel <- TRUE
        for (dim in names(grid)) {
          sel <- sel & (x[[dim]] > min(x[[dim]])) & (x[[dim]] < max(x[[dim]]))
        }
        return(x[sel, ])
      }) %>%
      dplyr::group_split() %>%
      dplyr::bind_rows()
    if (orig_len - nrow(vct) > max_prop * orig_len) {
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
    # if (any(is.na(vct[paste0(dim, "_coord")]))) {
    #   stop(paste0("create_PSD_one_file: ", dim, " value out of range in ", vct_file))
    # }
  }

  # Group by time, grid coordinates, and population
  # Count cells in each group and sum Qc
  if (use_data.table) {
    # data.table is much faster at this group by than dplyr, sometimes < 1s vs ~30s
    orig_threads <- data.table::getDTthreads()
    data.table::setDTthreads(1)  # turn off data.table multi-threading
    vct <- data.table::as.data.table(vct)
    coord_cols <- stringr::str_subset(names(vct), "_coord$")
    group_cols <- c("date", coord_cols, "pop")
    if ("Qc" %in% names(grid)) {
      vct_summary <- vct[, list(n=.N, Qc_sum=sum(Qc)), keyby=group_cols]
    } else {
      vct_summary <- vct[, list(n=.N), keyby=group_cols]
    }
    data.table::setDTthreads(orig_threads)  # reset data.table multi-threading
    vct_summary <- tibble::as_tibble(vct_summary)
  } else {
    vct_summary <- vct %>%
      dplyr::group_by(date, dplyr::across(ends_with("_coord")), pop)
    if ("Qc" %in% names(grid)) {
      vct_summary <- vct_summary %>%
        dplyr::summarise(n=dplyr::n(), Qc_sum=sum(Qc), .groups="drop")
    } else {
      vct_summary <- vct_summary %>%
        dplyr::summarise(n=dplyr::n(), .groups="drop")
    }
  }

  if (verbose) {
    message(paste(diag_text, collapse="\n"))
  }

  return(vct_summary)
}

#' Group gridded particle data at a lower time resolution
#'
#' @param psd Gridded data created by create_PSD().
#' @param time_expr Time expression passed to lubridate::floor_date to
#'   lower time resolution.
#' @param use_data.table Use data.table for performance speedup, otherwise use dplyr.
#' @return A tibble of psd with reduced time resolution.
#' @export
group_psd_by_time <- function(psd, time_expr="1 hours", use_data.table=TRUE) {
  # data.table is twice as fast for this operation as dplyr in testing on HOT310
  if (use_data.table) {
    # This is a side-effect, converting psd by reference to a data.table, and
    # as such this effect will persist after the functions exits. Not great, but
    # this object can very large and I'd rather not make a copy.
    psd <- data.table::setDT(psd)
    psd$date2 <- lubridate::floor_date(psd$date, time_expr)
    group_cols <- c("date2", stringr::str_subset(names(psd), "_coord$"), "pop")
    if ("Qc_sum" %in% names(psd)) {
      grouped <- psd[,
                    list(n=sum(n), Qc_sum=sum(Qc_sum)),
                    keyby=group_cols
                 ]
    } else {
      grouped <- psd[,
                    list(n=sum(n)),
                    keyby=group_cols
                 ]
    }
    grouped <- tibble::as_tibble(grouped)
    grouped <- grouped %>% rename(date = date2)
  } else {
    grouped <- psd %>%
      dplyr::group_by(date=lubridate::floor_date(date, time_expr), dplyr::across(ends_with("_coord")), pop) %>%
      dplyr::arrange(by_group=TRUE)
    if ("Qc_sum" %in% names(psd)) {
      grouped <- grouped %>%
        dplyr::summarise(n=sum(n), Qc_sum=sum(Qc_sum), .groups="drop")
    } else {
      grouped <- grouped %>%
        dplyr::summarise(n=sum(n), .groups="drop")
    }
  }
  return(grouped)
}

#' Add volume-normalized abundance to gridded SeaFlow data
#'
#' @param psd Gridded data created by create_PSD().
#' @param volumes Volume dataframe at the same time resolution as psd, usually created by
#'   create_volume_table().
#' @param calib Optional influx calibration dataframe used to adjust data by population.
#'   Columns should incude pop and a.
#' @return psd with volume-normalized abundance columns for n_per_uL and Qc_sum_per_uL.
#'   If calib is provided n and Qc_sum will be adjusted for populations in calib.
#' @export
add_adundance <- function(psd, volumes, calib=NULL) {
  # Calculate abundance
  psd <- dplyr::left_join(psd, volumes, by="date")
  pop_idx <- psd$pop == "prochloro" | psd$pop == "synecho"

  psd[, "n_per_uL"] <- psd[, "n"] / psd[, "volume_large"]
  psd[, "Qc_sum_per_uL"] <- psd[, "Qc_sum"] / psd[, "volume_large"]

  psd[pop_idx, "n_per_uL"] <- psd[pop_idx, "n"] / psd[pop_idx, "volume_small"]
  psd[pop_idx, "Qc_sum_per_uL"] <- psd[pop_idx, "Qc_sum"] / psd[pop_idx, "volume_small"]

  # Calibrate to influx data if provided
  if (!is.null(calib)) {
    if (nrow(calib) == 0) {
      stop("calibration table is empty")
    }
  
    popname <- unique(calib$pop)
    for (phyto in popname) {
      corr <- calib %>% dplyr::filter(pop == phyto)
      if (nrow(corr) > 1) {
        stop(paste0("more than one abundance calibration entry found for ", phyto))
      }
      if (nrow(corr) == 1){
      psd[psd$pop == phyto, "n_per_uL"] <- psd[psd$pop == phyto, "n_per_uL"] * corr[["a"]]
      psd[psd$pop == phyto, "Qc_sum_per_uL"] <- psd[psd$pop == phyto, "Qc_sum_per_uL"] * corr[["a"]]
      }
    }
  }

  psd <- psd %>%
    dplyr::select(-c(n, Qc_sum, volume, volume_small, volume_large))

  return(psd)
}

#' Create a metadata tibble for one quantile from the SFL table.
#'
#' @param db popcycle database file.
#' @param quantile OPP filtering quantile to use.
#' @return A tibble of date, volume, opp_evt_ratio, flag
#' @export
create_meta <- function(db, quantile) {
  quantile <- as.numeric(quantile)

  ### Retrieve metadata
  meta <- get_opp_table(db, sfl_join = TRUE, all_sfl_columns = TRUE, outlier_join = TRUE)
  meta <- meta[meta$quantile == quantile, ]
  meta$flag <- as.factor(meta$flag)

  # retrieve flow rate (mL min-1) of detectable volume
  fr <- flowrate(meta$stream_pressure, inst = get_inst(db))$flow_rate
  # convert to microL min-1
  fr <- fr * 1000
  # acquisition time (min)
  acq.time <- meta$file_duration / 60
  # volume in microL
  meta$volume <- round(fr * acq.time, 0)
  meta <- meta %>% dplyr::select(c("date", "volume", "opp_evt_ratio", "flag"))

  return(meta)
}

#' Create a tibble of volumes that can be used to calculate SeaFlow abundances
#'
#' @param meta Metadata dataframe created by create_meta()
#' @param time_expr Time expression passed to lubridate::floor_date to group datetimes
#'   before calculating volumes. If NULL volumes are returned at their original time
#'   resolution.
#' @return A tibble of volumes, per-file OPP/EVT ratio adjusted volumes for small
#'   particles, and global median OPP/EVT ratio adjusted volumes for large particles.
#' @export
create_volume_table <- function(meta, time_expr = "1 hour") {
  meta <- meta %>% dplyr::select(date, volume, opp_evt_ratio)
  meta <- meta %>%
    dplyr::mutate(
      volume_small = volume * opp_evt_ratio,
      volume_large = volume * median(opp_evt_ratio)
    )
  if (!is.null(time_expr)) {
    meta <- meta %>%
      dplyr::mutate(date = lubridate::floor_date(date, time_expr)) %>%
      dplyr::group_by(date) %>%
      dplyr::arrange(by_group = TRUE) %>%
      dplyr::summarise(
        volume = sum(volume),
        volume_small = sum(volume_small),
        volume_large = sum(volume_large)
      )
  } else {
    meta <- meta %>% select(-c(opp_evt_ratio))
  }

  return(tibble::as_tibble(meta))
}

#' Create the breaks to use to grid VCT data.
#'
#' @param bins Number of bins between breaks.
#' @param channel_range Vector of range to use for standard SeaFlow channel breaks.
#' @param Qc_range Vector of range to use for Qc breaks.
#' @param diam_range Vector of range to use for diam breaks.
#' #' @param log_base Log base to use when creating log-spaced bins for distribution.
#'   If NULL the breaks will not be log-spaced.
#' @param log_answers If log_base is provided, this determines whether the break
#'   values themselves are log values or not. e.g. if TRUE breaks may be
#'   [1, 2, 3], if FALSE breaks may be [10, 100, 1000].
#' @return Named list defining breaks for use with cut(). Each item has length == bins + 1.
#' @export
create_grid <- function(bins = 85, channel_range = c(1, 3200), Qc_range = c(0.002, 1600),
                        diam_range = c(0.1, 37), log_base = NULL, log_answers = TRUE) {
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

#' Create breaks vector that can be used with cut().
#'
#' @param bins Number of bins between breaks.
#' @param minval Value for the minimum break (fencepost).
#' @param maxval Value for the maximum break (fencepost).
#' @param log_base Log base to use when creating log-spaced bins for distribution.
#'   If NULL the breaks will not be log-spaced.
#' @param log_answers If log_base is provided, this determines whether the break
#'   values themselves are log values or not. e.g. if TRUE breaks may be
#'   [1, 2, 3], if FALSE breaks may be [10, 100, 1000].
#' @return Vector defining breaks for use with cut(), length == bins + 1
create_breaks <- function(bins, minval, maxval, log_base=NULL, log_answers=TRUE) {
  if (!is.null(log_base)) {
    minval <- log(minval, base = log_base)
    maxval <- log(maxval, base = log_base)
  }
  b <- seq(from = minval, to = maxval, length = bins+1)
  if (!is.null(log_base) && !log_answers) {
    return(log_base^b)
  }
  return(b)
}

#' Find min/max for a data column / quantile pair in VCT files
#'
#' @param vct_files Vector of VCT file paths.
#' @param data_cols Character vector of VCT columns to use as size metric:
#'  fsc_small, chl_small, pe, Qc_[lwr,mid,upr], or diam_[lwr,mid,upr].
#'  For Qc and diam the quantile will be added to the column name automatically.
#' @param quantile OPP Filtering quantile.
#' @param pops Single population label to filter for.
#' @param cores Number of cores to use
#' @return Two item numeric vector of c(min_val, max_val)
#' @export
get_vct_range <- function(vct_files, data_cols, quantile, pop = NULL, cores = 1) {
  ptm <- proc.time()
  # Get vector of file paths

  cores <- min(cores, parallel::detectCores())
  message("using ", cores, " cores")
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    answer <- foreach::foreach(vct_file = vct_files, .inorder = TRUE) %dopar% {
      return(get_vct_range_one_file(vct_file, data_cols, quantile, pop = pop, cores = cores))
    }
    parallel::stopCluster(cl)
  } else {
    # Serial code
    answer <- purrr::map(vct_files, ~ get_vct_range_one_file(., data_cols, quantile, pop = pop, cores = cores))
  }

  answer <- purrr::flatten_dbl(answer)
  answer <- answer[!is.infinite(answer)]
  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(c(min(answer), max(answer)))
}

#' Find min/max for a data column / quantile pair in one VCT file
#'
#' @param vct_file VCT file path.
#' @param data_cols Character vector of VCT columns to use as size metric:
#'  fsc_small, chl_small, pe, Qc_[lwr,mid,upr], or diam_[lwr,mid,upr].
#'  For Qc and diam the quantile will be added to the column name automatically.
#' @param quantile OPP Filtering quantile.
#' @param pops Single population label to filter for.
#' @param cores Number of cores to use
#' @return Two item numeric vector of c(min_val, max_val)
get_vct_range_one_file <- function(vct_file, data_cols, quantile, pop = NULL, cores = 1) {
  qstr <- paste0("q", as.numeric(quantile))
  qsuffix <- paste0("_", qstr)

  # Check data column requested for validity and for presence of multiple refractive indexes
  refractive_cols <- c("Qc_lwr", "Qc_mid", "Qc_upr", "diam_lwr", "diam_mid", "diam_upr")
  channel_cols <- c("fsc_small", "chl_small", "pe")

  if (!all(data_cols %in% c(refractive_cols, channel_cols))) {
    stop(paste("data_col must be one of", paste(channel_cols, collapse=" "), paste(refractive_cols, collapse=" ")))
  }
  refractive <- data_cols[data_cols %in% refractive_cols]
  not_refractive <- data_cols[!(data_cols %in% refractive_cols)]

  final_cols <- c(qstr)
  if (length(refractive) > 0) {
    final_cols <- c(final_cols, paste0(refractive, qsuffix))
  }
  if (length(not_refractive) > 0) {
    final_cols <- c(final_cols, not_refractive)
  }
  if (!is.null(pop)) {
    pop_col <- paste0("pop", qsuffix)
    final_cols <- c(final_cols, pop_col)
  }
  vct <- arrow::read_parquet(
    vct_file,
    col_select = c(all_of(final_cols))
  )
  if (!is.null(pop)) {
    vct <- vct[vct[[pop_col]] == pop, ]
  }
  # Find min and max for each data column
  # This will warn about no non-missing arguments if no data for this file
  # result will be Inf or -Inf in this case, filter these values out later
  minmax <- vct %>%
    dplyr::filter(get(qstr)) %>%
    dplyr::select(where(is.numeric)) %>%
    suppressWarnings(dplyr::summarise_all(range))
  result <- suppressWarnings(c(min(as.matrix(minmax)), max(as.matrix(minmax))))
  return(result)
}

#' Find quantiles for VCT data
#'
#' @param vct_files VCT file paths.
#' @param data_cols Character vector of VCT column to use as size metric:
#'  fsc_small, chl_small, pe, Qc_[lwr,mid,upr], or diam_[lwr,mid,upr].
#'  For Qc and diam the filtering quantile will be added to the column name
#'  automatically.
#' @param filtering_quantile OPP Filtering quantile.
#' @param quantile_probs Numeric vector of probabilities with values in [0,1].
#' @param pops Single population label to filter for.
#' @param ignore_dates Don't process VCT data with these dates.
#' @return Result of quantile()
#' @export
get_vct_quantile_range <- function(vct_files, col, filtering_quantile, quantile_probs = c(.01, .99), 
                                   pop = NULL, ignore_dates = NULL) {
  qstr <- paste0("q", filtering_quantile)
  qsuffix <- paste0("_", qstr)
  data_col <- paste0(col, qsuffix)
  pop_col <- paste0("pop", qsuffix)
  need_cols <- c(qstr, data_col)
  if (!is.null(ignore_dates)) {
    need_cols <- c("date", need_cols)
  }
  if (!is.null(pop)) {
    need_cols <- c(need_cols, pop_col)
    data <- dplyr::bind_rows(lapply(vct_files, function(f) {
      arrow::read_parquet(f, col_select = all_of(need_cols)) %>%
        dplyr::filter(.data[[pop_col]] == pop, .data[[qstr]] == TRUE)
    }))
  } else {
    data <- dplyr::bind_rows(lapply(vct_files, function(f) {
      arrow::read_parquet(f, col_select = all_of(need_cols)) %>%
        dplyr::filter(.data[[qstr]] == TRUE)
    }))
  }
  # Ignore certain dates
  if (!is.null(ignore_dates)) {
    data <- data %>% dplyr::filter(! (date %in% ignore_dates))
  }

  return(quantile(data[[data_col]], quantile_probs))
}

#' Show messages for rows in gridded particle data that fail validation
#'
#' @param psd Gridded dataframe created by create_PSD. This function loops through
#'   rows in this dataframe so provide a subset of a real psd dataframe for testing.
#' @param vct VCT dataframe that covers time ranges present in psd.
#' @param grid The grid named list used to create psd.
#' @param refrac Dataframe of population-specific refractive indices used to create psd.
validate_psd <- function(psd, vct, grid, refrac) {
  qc_col <- paste0("Qc_", refrac)
  for (i in seq_along(psd$date)) {
    psdr <- psd[i, ]  # one row of psd data
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

    msg <- paste(c(paste0("psd_n=", psdr$n), paste0("vct_n=", nrow(vct_match)), as.character(psdr$date), psdr$fsc_small_coord, psdr$pe_coord, psdr$chl_small_coord, psdr$Qc_coord, as.character(psdr$pop)), collapse=" ")
    if (psdr$n != nrow(vct_match)) {
      msg <- paste0("MISMATCH: ", msg)
    }
    print(msg)

    vct_Qc_sum <- sum(vct_match$Qc)
    msg <- paste(c(paste0("psd_Qc_sum=", psdr$Qc_sum), paste0("vct_Qc_sum=", vct_Qc_sum), as.character(psdr$date), psdr$fsc_small_coord, psdr$pe_coord, psdr$chl_small_coord, psdr$Qc_coord, as.character(psdr$pop)), collapse=" ")
    if (psdr$Qc_sum != vct_Qc_sum) {
      msg <- paste0("MISMATCH: ", msg)
    }
    print(msg)
  }
}

#' Construct string labels for break points created by create_grid()
#'
#' @param grid Named list of breakpoints created by create_grid().
#' @return Named list identical to grid with numeric break points converted to strings.
#' @export
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

#' Add string coordinate labels to a dataframe of gridded SeaFlow data
#'
#' @param psd Gridded dataframe created by create_PSD.
#' @param grid Named list of breakpoints created by create_grid().
#' @return Named list identical to grid with numeric break points converted to character vectors.
#' @export
add_coord_labels <- function(psd, grid) {
  labels <- grid_labels(grid)
  for (col in colnames(psd)) {
    if (endsWith(col, "_coord")) {
      colbase <- stringr::str_replace(col, "_coord$", "")
      newcol <- paste0(colbase, "_label")
      psd <- psd %>%
        tibble::add_column("{newcol}" := labels[[colbase]][psd[[col]]], .after = col)
    }
  }
  return(psd)
}
