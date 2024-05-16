#' Create gridded distribution data as particle counts and Qc sums for many VCT files
#'
#' @param vct_files VCT files to grid.
#' @param quantile OPP filtering quantile.
#' @param refracs Dataframe of refractive indices to use for each population. Column names
#'   should be population names used in classification, values should be mid, lwr, upr.
#' @param grid_bins Named list of break points to use for gridding. Names should be fsc_small, pe,
#'   chl_small, Qc, diam.
#' @param log_base If the break points are logged values, provide the base here to properly
#'   log the VCT data before gridding. If break points are not log use NULL.
#' @param max_boundary_proportion Proportion of particles in each VCT file which can be
#'   removed as boundary points before discarding all data in the file.
#'   Should be > 0 and <= 1. If this parameter is NULL, no boundary points will be removed
#'   before gridding.
#' @param ignore_dates Don't process VCT data with these dates.
#' @param pop A single populations label to keep. Particles not matching this label will be
#'   ignored.
#' @param use_data.table Use data.table for performance speedup, otherwise use dplyr.
#' @param cores Number of cores to use
#' @return Tibble of gridded data, grouped by date, fsc_small, pe, chl_small, Qc diam grid
#'   locations, and population. Each grid coordinate is an integer index into the corresponding
#'   grid breaks vector. The break point at that location defines the inclusive lower bound
#'   of the bin, and the next break point in the sequence defines the exclusive upper bound.
#'   Data columns summarizing each group are n for particle count and Qc_sum for the the sum
#'   of Qc.
#' @export
create_gridded <- function(vct_files, quantile, refracs, grid_bins, log_base = NULL, max_boundary_proportion = NULL,
                           ignore_dates = NULL, pop = NULL, use_data.table = TRUE, cores = 1) {
  ptm <- proc.time()
  quantile <- as.numeric(quantile)
  cores <- min(cores, parallel::detectCores())
  message("using ", cores, " cores")
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    counts_list <- foreach::foreach(v = vct_files, .inorder = TRUE) %dopar% {
      grid_one_file(v, quantile, refracs, grid_bins, log_base=log_base,
                    max_boundary_proportion=max_boundary_proportion,
                    use_data.table=use_data.table, ignore_dates=ignore_dates,
                    pop=pop)
    }
    parallel::stopCluster(cl)
  } else {
    # Serial code
    counts_list <- lapply(vct_files, function(v) {
      grid_one_file(v, quantile, refracs, grid_bins, log_base=log_base,
                    max_boundary_proportion=max_boundary_proportion,
                    use_data.table=use_data.table, ignore_dates=ignore_dates,
                    pop=pop)
    })
  }
  counts_list <- counts_list %>% purrr::discard(is.null)
  counts <- dplyr::bind_rows(counts_list)
  rm(counts_list)
  invisible(gc())
  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(counts)
}

#' Create gridded distribution data as particle counts and Qc sums for one VCT file
#'
#' @param vct_file VCT file path.
#' @param quantile OPP filtering quantile.
#' @param refracs Dataframe of refractive indices to use for each population. Column names
#'   should be population names used in classification, values should be mid, lwr, upr.
#' @param grid_bins Named list of break points to use for gridding. Names should be fsc_small, pe,
#'   chl_small, Qc, diam.
#' @param log_base If the break points are logged values, provide the base here to properly
#'   log the VCT data before gridding. If break points are not log use NULL.
#' @param max_boundary_proportion Proportion of particles in each VCT file which can be
#'   removed as boundary points before discarding all data in the file.
#'   Should be > 0 and <= 1. If this parameter is NULL, no boundary points will be removed
#'   before gridding.
#' @param ignore_dates Don't process VCT data with these dates.
#' @param pop A single population label to keep. Particles not matching this label will be
#'   ignored.
#' @param use_data.table Use data.table for performance speedup, otherwise use dplyr.
#' @return Tibble of gridded data, grouped by date, fsc_small, pe, chl_small, Qc diam grid
#'   locations, and population. Each grid coordinate is an integer index into the corresponding
#'   grid breaks vector. The break point at that location defines the inclusive lower bound
#'   of the bin, and the next break point in the sequence defines the exclusive upper bound.
#'   Data columns summarizing each group are n for particle count and Qc_sum for the the sum
#'   of Qc. Return NULL if not data remains after removing boundary points,
#'   filtering by ignore_dates, or filtering by population.
grid_one_file <- function(vct_file, quantile, refracs, grid_bins, log_base = NULL, max_boundary_proportion = NULL,
                          ignore_dates = NULL, pop = NULL, use_data.table = TRUE) {
  if (!is.null(max_boundary_proportion) && ((max_boundary_proportion <= 0) || (max_boundary_proportion > 1))) {
    stop("max_boundary_proportion must be > 0 and <= 1")
  }

  # Read VCT data, applying proper refractive index to each population
  vct <- read_parquet_one_quantile(
    vct_file, quantile, c("date", "pop", names(grid_bins)), refracs = refracs
  )
  # Filter to single population
  if (!is.null(pop)) {
    vct <- vct %>% dplyr::filter(pop == .env[["pop"]])
  }
  # Ignore certain dates
  if (!is.null(ignore_dates)) {
    vct <- vct %>% dplyr::filter(! (date %in% ignore_dates))
  }
  if (nrow(vct) == 0) {
    return(NULL)
  }

  # Remove particles at the max value for any dimension
  if (!is.null(max_boundary_proportion)) {
    orig_len <- nrow(vct)
    vct <- remove_boundary_points(vct, names(grid_bins))
    boundary_removed <- orig_len - nrow(vct)
    if (boundary_removed > (max_boundary_proportion * orig_len)) {
      perc_removed <- (boundary_removed / orig_len) * 100
      message(paste0(
        "> ", max_boundary_proportion * 100, "% boundary points removed (", boundary_removed, "/", orig_len, " ", perc_removed, "%) in ", vct_file
      ))
      return(NULL)
    }
  }

  # Assign each particle to a cell in the grid for each dimension
  for (dim in names(grid_bins)) {
    if (is.null(log_base)) {
      values <- vct[[dim]]
    } else {
      values <- log(vct[[dim]], log_base)
    }

    # Label by index into grid
    vct[paste0(dim, "_coord")] <- as.integer(cut(values, grid_bins[[dim]], labels=FALSE, right=FALSE))
    # if (any(is.na(vct[paste0(dim, "_coord")]))) {
    #   stop(paste0("grid_one_file: ", dim, " value out of range in ", vct_file))
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
    if ("Qc" %in% names(grid_bins)) {
      vct_summary <- vct[, list(n=.N, Qc_sum=sum(Qc)), keyby=group_cols]
    } else {
      vct_summary <- vct[, list(n=.N), keyby=group_cols]
    }
    data.table::setDTthreads(orig_threads)  # reset data.table multi-threading
    vct_summary <- tibble::as_tibble(vct_summary)
  } else {
    vct_summary <- vct %>%
      dplyr::group_by(date, dplyr::across(ends_with("_coord")), pop)
    if ("Qc" %in% names(grid_bins)) {
      vct_summary <- vct_summary %>%
        dplyr::summarise(n=dplyr::n(), Qc_sum=sum(Qc), .groups="drop")
    } else {
      vct_summary <- vct_summary %>%
        dplyr::summarise(n=dplyr::n(), .groups="drop")
    }
  }

  return(vct_summary)
}

#' Remove boundary points.
#'
#' Boundary points are defined as points where the value for any of the
#' selected columns is equal to the maximum or minimum value for that column
#' at that date.
#'
#' @param df Dataframe of particle data with a date column
#' @param cols Columns to use for boundary point definition
#' @param max_only Only remove maximum boundary points
#' @return df with boundary points removed.
#' @export
remove_boundary_points <- function(df, cols, max_only = FALSE) {
  if (!all(cols %in% colnames(df))) {
    stop("not all columns found in dataframe")
  }
  if (!("date" %in% names(df))) {
    stop("date must be a column in df")
  }
  df <- df %>%
    dplyr::group_by(date) %>%
    dplyr::group_modify(function(x, y) {
      sel <- TRUE
      for (col in cols) {
        if (max_only) {
          sel <- sel & (x[[col]] < max(x[[col]]))
        } else {
          sel <- sel & (x[[col]] > min(x[[col]])) & (x[[col]] < max(x[[col]]))
        }
      }
      return(x[sel, ])
    }) %>%
    dplyr::group_split() %>%
    dplyr::bind_rows()
  return(df)
}

#' Group gridded particle data at a lower time resolution
#'
#' @param gridded Gridded data created by grid().
#' @param time_expr Time expression passed to lubridate::floor_date to
#'   lower time resolution.
#' @param use_data.table Use data.table for performance speedup, otherwise use dplyr.
#' @return A tibble of gridded with reduced time resolution.
#' @export
group_gridded_by_time <- function(gridded, time_expr="1 hours", use_data.table=TRUE) {
  # data.table is twice as fast for this operation as dplyr in testing on HOT310
  if (use_data.table) {
    # This is a side-effect, converting gridded by reference to a data.table, and
    # as such this effect will persist after the functions exits. Not great, but
    # this object can very large and I'd rather not make a copy.
    gridded <- data.table::setDT(gridded)
    gridded$date2 <- lubridate::floor_date(gridded$date, time_expr)
    group_cols <- c("date2", stringr::str_subset(names(gridded), "_coord$"), "pop")
    if ("Qc_sum" %in% names(gridded)) {
      grouped <- gridded[,
                    list(n=sum(n), Qc_sum=sum(Qc_sum)),
                    keyby=group_cols
                 ]
    } else {
      grouped <- gridded[,
                    list(n=sum(n)),
                    keyby=group_cols
                 ]
    }
    grouped <- tibble::as_tibble(grouped)
    grouped <- grouped %>% rename(date = date2)
  } else {
    grouped <- gridded %>%
      dplyr::group_by(date=lubridate::floor_date(date, time_expr), dplyr::across(ends_with("_coord")), pop) %>%
      dplyr::arrange(by_group=TRUE)
    if ("Qc_sum" %in% names(gridded)) {
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
#' @param gridded Gridded data created by grid().
#' @param volumes Volume dataframe at the same time resolution as gridded, usually created by
#'   create_volume_table().
#' @param calib Optional influx calibration dataframe used to adjust data by population.
#'   Columns should incude pop and a.
#' @return gridded with volume-normalized abundance columns for n_per_uL and Qc_sum_per_uL.
#'   If calib is provided n and Qc_sum will be adjusted for populations in calib.
#' @export
add_abundance <- function(gridded, volumes, calib=NULL) {
  # Calculate abundance
  gridded <- dplyr::left_join(gridded, volumes, by="date")
  gridded[, "n_per_uL"] <- gridded[, "n"] / gridded[, "volume_virtualcore"]
  gridded[, "Qc_sum_per_uL"] <- gridded[, "Qc_sum"] / gridded[, "volume_virtualcore"]
  # Calculate pro abund using per-file opp_evt_ratio (meaning per-file virtualcore volume)
  proindex <- gridded$pop == "prochloro"
  gridded[proindex, "n_per_uL"] <- gridded[proindex, "n"] / gridded[proindex, "volume_virtualcore_by_file"]
  gridded[proindex, "Qc_sum_per_uL"] <- gridded[proindex, "Qc_sum"] / gridded[proindex, "volume_virtualcore_by_file"]

  # Calibrate to influx data if provided
  if (!is.null(calib)) {
    if (nrow(calib) == 0) {
      stop("calibration table is empty")
    }

    for (phyto in unique(calib$pop)) {
      corr <- calib %>% dplyr::filter(pop == phyto)
      if (nrow(corr) > 1) {
        stop(paste0("more than one abundance calibration entry found for ", phyto))
      }
      if (nrow(corr) == 1) {
        popindex <- gridded$pop == phyto
        gridded[popindex, "n_per_uL"] <- gridded[popindex, "n_per_uL"] * corr[["a"]]
        gridded[popindex, "Qc_sum_per_uL"] <- gridded[popindex, "Qc_sum_per_uL"] * corr[["a"]]
      }
    }
  }

  gridded <- gridded %>%
    dplyr::select(-c(volume, volume_virtualcore, volume_virtualcore_by_file))

  return(gridded)
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
  meta$volume <- fr * acq.time
  meta <- meta %>% dplyr::select(c("date", "volume", "opp_evt_ratio", "flag"))

  return(meta)
}

#' Create a tibble of volumes that can be used to calculate SeaFlow abundances
#'
#' @param meta Metadata dataframe created by create_meta()
#' @param time_expr Time expression passed to lubridate::floor_date to group datetimes
#'   before calculating volumes. If NULL volumes are returned at their original time
#'   resolution.
#' @param median_opp_evt_ratio Fixed ratio to use.
#' @return A tibble of sample and virtual core volumes.
#' @export
create_volume_table <- function(meta, time_expr = "1 hour", median_opp_evt_ratio = NULL) {
  meta <- meta %>% dplyr::select(date, volume, opp_evt_ratio)
  if (is.null(median_opp_evt_ratio)) {
    meta$volume_virtualcore <- meta$volume * median(meta$opp_evt_ratio)
  } else {
    meta$volume_virtualcore <- meta$volume * median_opp_evt_ratio
  }
  meta$volume_virtualcore_by_file <- meta$volume * meta$opp_evt_ratio
  if (!is.null(time_expr)) {
    meta <- meta %>%
      dplyr::mutate(date = lubridate::floor_date(date, time_expr)) %>%
      dplyr::group_by(date) %>%
      dplyr::arrange(by_group = TRUE) %>%
      dplyr::summarise(
        volume = sum(volume),
        volume_virtualcore = sum(volume_virtualcore),
        volume_virtualcore_by_file = sum(volume_virtualcore_by_file)
      )
  } else {
    meta <- meta %>% select(-c(opp_evt_ratio))
  }

  return(tibble::as_tibble(meta))
}

#' Create the breaks to use to grid VCT data.
#'
#' @param bin_count Number of bins between breaks.
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
create_grid_bins <- function(bin_count = 85, channel_range = c(1, 3200), Qc_range = c(0.002, 1600),
                             diam_range = c(0.1, 37), log_base = NULL, log_answers = TRUE) {
  if (length(channel_range) != 2) {
    stop("create_grid_bins: channel_range must be a two-item numeric list or vector")
  }
  if (length(Qc_range) != 2) {
    stop("create_grid_bins: Qc_range must be a two-item numeric list or vector")
  }
  if (length(diam_range) != 2) {
    stop("create_grid_bins: diam_range must be a two-item numeric list or vector")
  }
  grid_bins <- list()
  grid_bins$fsc_small <- create_breaks(bin_count, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid_bins$pe <- create_breaks(bin_count, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid_bins$chl_small <- create_breaks(bin_count, channel_range[[1]], channel_range[[2]], log_base, log_answers)
  grid_bins$Qc <- create_breaks(bin_count, Qc_range[[1]], Qc_range[[2]], log_base, log_answers)
  grid_bins$diam <- create_breaks(bin_count, diam_range[[1]], diam_range[[2]], log_base, log_answers)
  return(grid_bins)
}

#' Create breaks vector that can be used with cut().
#'
#' @param bin_count Number of bins between breaks.
#' @param minval Value for the minimum break (fencepost).
#' @param maxval Value for the maximum break (fencepost).
#' @param log_base Log base to use when creating log-spaced bins for distribution.
#'   If NULL the breaks will not be log-spaced.
#' @param log_answers If log_base is provided, this determines whether the break
#'   values themselves are log values or not. e.g. if TRUE breaks may be
#'   [1, 2, 3], if FALSE breaks may be [10, 100, 1000].
#' @return Vector defining breaks for use with cut(), length == bins + 1
create_breaks <- function(bin_count, minval, maxval, log_base=NULL, log_answers=TRUE) {
  if (!is.null(log_base)) {
    minval <- log(minval, base = log_base)
    maxval <- log(maxval, base = log_base)
  }
  b <- seq(from = minval, to = maxval, length = bin_count+1)
  if (!is.null(log_base) && !log_answers) {
    return(log_base^b)
  }
  return(b)
}


#' Find min/max for a data column / quantile pair in VCT files
#'
#' @param vct_files Vector of VCT file paths.
#' @param data_cols Character vector of VCT columns to use as size metric:
#'  fsc_small, chl_small, pe, Qc, or diam.
#'  For Qc and diam, refractive index alias and quantile will be added to the
#'  column name automatically. If refracs is provided, the appropriate refractive
#'  index will automatically be applied to Qc and diam and only Qc and diam will
#'  be returned as column names if requested. i.e. there will be no refractive
#'  index alias suffix such as "_lwr".
#' @param quantile OPP Filtering quantile.
#' @param pop Single population label to filter for.
#' @param ignore_dates Don't process VCT data with these dates.
#' @param cores Number of cores to use
#' @param refracs Dataframe of refractive indices to use for each population. Column names
#'   should be population names used in classification, values should be mid, lwr, upr.
#'   If NULL all refractive indexes will be used.
#' @return Two item numeric vector of c(min_val, max_val)
#' @export
get_vct_range <- function(vct_files, data_cols, quantile, pop = NULL,
                          ignore_dates = NULL, refracs = NULL, cores = 1) {
  ptm <- proc.time()
  # Get vector of file paths

  cores <- min(cores, parallel::detectCores())
  message("using ", cores, " cores")
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    answer <- foreach::foreach(vct_file = vct_files, .inorder = TRUE) %dopar% {
      return(get_vct_range_one_file(vct_file, data_cols, quantile, pop = pop,
                                    ignore_dates = ignore_dates,
                                    refracs = refracs, cores = cores)
      )
    }
    parallel::stopCluster(cl)
  } else {
    # Serial code
    answer <- purrr::map(
      vct_files,
      ~ get_vct_range_one_file(., data_cols, quantile, pop = pop,
                               ignore_dates = ignore_dates, refracs = refracs,
                               cores = cores))
  }

  df <- dplyr::bind_rows(answer)
  deltat <- proc.time() - ptm
  # Results from individual files may have Inf or -Inf if all data was filtered
  # out before getting the range, so use finite = TRUE in range call here to
  # to exclude those values.
  minmax <- df %>%
    dplyr::reframe(across(everything(), ~ suppressWarnings(range(., finite = TRUE))))
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(minmax)
}

#' Find min/max for a data column / quantile pair in one VCT file
#'
#' @param vct_file VCT file path.
#' @param data_cols Character vector of VCT columns to use as size metric:
#'  fsc_small, chl_small, pe, Qc_[lwr,mid,upr], or diam_[lwr,mid,upr].
#'  For Qc and diam the quantile will be added to the column name automatically.
#' @param quantile OPP Filtering quantile.
#' @param pop Single population label to filter for.
#' @param ignore_dates Don't process VCT data with these dates.
#' @param cores Number of cores to use
#' @return Two item numeric vector of c(min_val, max_val)
get_vct_range_one_file <- function(vct_file, data_cols, quantile, pop = NULL,
                                   ignore_dates = NULL, refracs = NULL,
                                   cores = 1) {
  possible_cols <- c("fsc_small", "chl_small", "pe", "diam", "Qc")
  if (!all(data_cols %in% possible_cols)) {
    stop(paste("data_col must be one of", paste(possible_cols, collapse=" ")))
  }
  if (!is.null(ignore_dates) && !("date" %in% data_cols)) {
    data_cols <- c("date", data_cols)
  }
  if (!is.null(pop) && !("pop" %in% data_cols)) {
    data_cols <- c(data_cols, "pop")
  }

  vct <- popcycle::read_parquet_one_quantile(vct_file, quantile, cols = data_cols, refracs = refracs)
  if (!is.null(pop)) {
    vct <- vct %>% dplyr::filter(pop == .env[["pop"]])
  }
  # Ignore certain dates
  if (!is.null(ignore_dates)) {
    vct <- vct %>% dplyr::filter(! (date %in% ignore_dates))
  }
  # Find min and max for each data column
  # This will warn about no non-missing arguments if no data for this file
  # result will be Inf or -Inf in this case, filter these values out later
  minmax <- vct %>%
    dplyr::reframe(across(where(is.numeric), ~ suppressWarnings(range(.))))
  return(minmax)
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
#' @param pop Single population label to filter for.
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
#' @param gridded Gridded dataframe created by grid(). This function loops through
#'   rows in this dataframe so provide a subset of a real gridded dataframe for testing.
#' @param vct VCT dataframe that covers time ranges present in gridded.
#' @param grid_bins The grid bins named list used to create gridded.
#' @param refrac Dataframe of population-specific refractive indices used to create gridded.
validate_gridded <- function(gridded, vct, grid_bins, refrac) {
  qc_col <- paste0("Qc_", refrac)
  for (i in seq_along(gridded$date)) {
    griddedr <- gridded[i, ]  # one row of gridded data
    vct_match <- vct %>%
      dplyr::filter(
        date == griddedr$date,
        fsc_small >= grid$fsc_small[griddedr$fsc_small_coord],
        fsc_small < grid$fsc_small[griddedr$fsc_small_coord + 1],
        pe >= grid$pe[griddedr$pe_coord],
        pe < grid$pe[griddedr$pe_coord + 1],
        chl_small >= grid$chl_small[griddedr$chl_small_coord],
        chl_small < grid$chl_small[griddedr$chl_small_coord + 1],
        !!dplyr::sym(qc_col) >= grid$Qc[griddedr$Qc_coord],
        !!dplyr::sym(qc_col) < grid$Qc[griddedr$Qc_coord + 1],
        pop == griddedr$pop
      )

    msg <- paste(c(paste0("gridded_n=", griddedr$n), paste0("vct_n=", nrow(vct_match)), as.character(griddedr$date), griddedr$fsc_small_coord, griddedr$pe_coord, griddedr$chl_small_coord, griddedr$Qc_coord, as.character(griddedr$pop)), collapse=" ")
    if (griddedr$n != nrow(vct_match)) {
      msg <- paste0("MISMATCH: ", msg)
    }
    print(msg)

    vct_Qc_sum <- sum(vct_match$Qc)
    msg <- paste(c(paste0("gridded_Qc_sum=", griddedr$Qc_sum), paste0("vct_Qc_sum=", vct_Qc_sum), as.character(griddedr$date), griddedr$fsc_small_coord, griddedr$pe_coord, griddedr$chl_small_coord, griddedr$Qc_coord, as.character(griddedr$pop)), collapse=" ")
    if (griddedr$Qc_sum != vct_Qc_sum) {
      msg <- paste0("MISMATCH: ", msg)
    }
    print(msg)
  }
}

#' Construct string labels for break points created by create_grid_bins()
#'
#' @param grid_bins Named list of breakpoints created by create_grid_bins().
#' @return Named list identical to grid with numeric break points converted to strings.
#' @export
grid_bins_labels <- function(grid_bins) {
  labels <- list()
  for (channel in names(grid_bins)) {
    g <- grid_bins[[channel]]
    labels[[channel]] <- sapply(seq(1, length(g)-2), function(i) { paste0("[", format(round(g[i], 4), nsmall = 4), "-", format(round(g[i+1], 4), nsmall = 4), ")") })
    i <- length(g) - 1
    labels[[channel]] <- c(labels[[channel]], paste0("[", format(round(g[i], 4), nsmall = 4), "-", format(round(g[i+1], 4), nsmall = 4), "]"))
  }

  return(labels)
}

#' Add string coordinate labels to a dataframe of gridded SeaFlow data
#'
#' @param gridded Gridded dataframe created by create_gridded.
#' @param grid_bins Named list of breakpoints created by create_grid_bins().
#' @return Named list identical to grid with numeric break points converted to character vectors.
#' @export
add_coord_labels <- function(gridded, grid_bins) {
  labels <- grid_labels(grid_bins)
  for (col in colnames(gridded)) {
    if (endsWith(col, "_coord")) {
      colbase <- stringr::str_replace(col, "_coord$", "")
      newcol <- paste0(colbase, "_label")
      gridded <- gridded %>%
        tibble::add_column("{newcol}" := labels[[colbase]][gridded[[col]]], .after = col)
    }
  }
  return(gridded)
}
