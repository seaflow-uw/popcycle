#' Filter EVT particles.
#'
#' @param evt EVT data frame.
#' @param filter_params Filtering parameters in data frame. Should contain
#'   parameters for all quantiles. Columns should include quantile, width,
#'   notch.small.D1, notch.small.D2, notch.large.D1, notch.large.D2,
#'   offset.small.D1, offset.small.D2, offset.large.D1, offset.large.D2.
#' @return OPP data frame with new logical columns for each quantile.
#' @export
filter_evt <- function(evt, filter_params) {
  was_tibble <- FALSE
  if ("tbl_df" %in% class(evt)) {
    evt <- as.data.frame(evt)
  }

  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    if (was_tibble) {
      return(tibble::tibble())
    } else {
      return(data.frame(c()))
    }
  }

  # linearize the LOG transformed data
  lin <- FALSE
  if (is_transformed(evt)) {
    evt <- untransformData(evt)
    lin <- TRUE
  }
  # Assume width the same for all quantiles
  if (length(unique(filter_params[, "width"])) != 1) {
    stop("Not exactly one width in filter parameters")
  }
  width <- as.numeric(filter_params[1, "width"])

  # Filtering out noise
  noise <- identify_noise(evt)
  # Filtering out particles with saturated  D1 or D2 signal
  # Here this means both D1 AND D2 must be below the max for either channel
  saturated <- identify_saturated(evt)
  # Filtering aligned particles (D1 = D2)
  aligned_selector <- (!noise) & (!saturated) & (evt$D2 < evt$D1 + width) & (evt$D1 < evt$D2 + width)
  # Track which particles are OPP in any quantile
  opp_selector <- FALSE

  for (quantile in QUANTILES) {
    p <- filter_params[filter_params$quantile == quantile, ]
    notch.small.D1 <- as.numeric(p$notch.small.D1)
    notch.small.D2 <- as.numeric(p$notch.small.D2)
    notch.large.D1 <- as.numeric(p$notch.large.D1)
    notch.large.D2 <- as.numeric(p$notch.large.D2)
    offset.small.D1 <- as.numeric(p$offset.small.D1)
    offset.small.D2 <- as.numeric(p$offset.small.D2)
    offset.large.D1 <- as.numeric(p$offset.large.D1)
    offset.large.D2 <- as.numeric(p$offset.large.D2)

    # Filtering focused particles (fsc_small > D + notch)
    qopp_selector <- (((evt$D1 <= evt$fsc_small * notch.small.D1 + offset.small.D1) &
      (evt$D2 <= evt$fsc_small * notch.small.D2 + offset.small.D2)) |
      ((evt$D1 <= evt$fsc_small * notch.large.D1 + offset.large.D1) &
      (evt$D2 <= evt$fsc_small * notch.large.D2 + offset.large.D2))) &
      aligned_selector

    # Mark focused particles for this quantile in the original EVT dataframe
    qcolumn <- paste0("q", quantile)
    evt[qcolumn] <- qopp_selector

    # Build a logical row selector which finds particles which are focused in
    # any quantile.
    opp_selector <- opp_selector | qopp_selector
  }

  opp <- evt[opp_selector, ]  # select for focused particles

  if (lin) {
    opp <- transformData(opp)
  }

  if (was_tibble) {
    opp <- tibble::as_tibble(opp)
  }

  return(opp)
}

#' Return logical index vector for noise particle rows
#' @param evt EVT data frame
#' @return Logical index vector for noise particle rows
identify_noise <- function(evt) {
  return((evt$fsc_small <= 1) & (evt$D1 <= 1) & (evt$D2 <= 1))
}

#' Return logical index vector for saturated particle rows
#' @param evt EVT data frame
#' @return Logical index vector for saturated particle rows
identify_saturated <- function(evt) {
  return((evt$D1 == max(evt$D1)) | (evt$D2 == max(evt$D2)))
}

#' Filter a list of EVT files.
#'
#' Filter a list of EVT files. Save OPP per file aggregate statistics to
#' SQLite3 database and save particle data to hourly parquet files in opp_dir.
#' Set outlier flag to 0 for all files without a current entry in the outlier
#' table, otherwise leave the outlier entry as is.
#'
#' @param db SQLite3 database file path.
#' @param evt_dir EVT file directory.
#' @param evt_files List of EVT file IDs to filter. Include julian day directory.
#'   Pass NULL to filter all EVT files discovered in evt_dir.
#' @param opp_dir OPP file output directory.
#' @param filter_id Optionally provide the ID for filter parameters. Pass NULL
#'   to filter using schedule describedin the filter_plan table.
#' @param max_particles_per_file Maximum number of particles per 3 minute EVT file.
#'   Set to NULL to disable.
#' @param max_opp_per_file Maximum OPP per 3 minute EVT file.
#'   Set to NULL to disable.
#' @param cores Number of logical cores to use.
#' @return None
#' @export
filter_evt_files <- function(db, evt_dir, evt_files, opp_dir, filter_id = NULL,
                             max_particles_per_file = NULL,
                             max_opp_per_file = NULL,
                             cores = 1) {
  ptm <- proc.time()

  # Normalize evt_dir to make sure all paths are comparable
  evt_dir <- normalizePath(evt_dir, mustWork=TRUE)

  plan <- create_full_filter_plan(evt_files, db, evt_dir, opp_dir, filter_id = filter_id)

  message(nrow(plan), " files to filter")

  if (nrow(plan) > 0) {
    # Get all filter params for easy lookup later without talking to the db every
    # file_id
    filter_params <- list()
    for (fid in unique(plan$filter_id)) {
      filter_params[[fid]] <- get_filter_params_by_id(db, fid)
    }

    # Perform filtering, save new OPP Parquet files
    # Group by input time-windowed OPP Parquet file path. Filtering will be
    # performed for all file IDs in one parquet file at a time.
    by_window_opp_path <- dplyr::group_by(
      plan,
      window_opp_path
    )
    cores <- min(cores, parallel::detectCores())
    message("using ", cores, " cores")
    if (cores > 1) {
      # Parallel code
      cl <- parallel::makeCluster(cores, outfile="")
      doParallel::registerDoParallel(cl)
      windows <- dplyr::group_split(by_window_opp_path)
      answer <- foreach::foreach(df = windows, .inorder = TRUE) %dopar% {
        filter_window_evt(df, NULL, filter_params, max_particles_per_file = max_particles_per_file,
                          max_opp_per_file = max_opp_per_file)
      }
      parallel::stopCluster(cl)
    } else {
      # Serial code
      answer <- dplyr::group_map(
        by_window_opp_path,
        filter_window_evt,
        filter_params,
        max_particles_per_file = max_particles_per_file,
        max_opp_per_file = max_opp_per_file,
        .keep=TRUE
      )
    }
    # Save opp stats to DB
    opp_stats_all <- dplyr::bind_rows(lapply(answer, function(item) item$opp_stats)) %>%
      dplyr::select(-c(date))
    save_opp_stats(db, opp_stats_all)
    # Save outlier status to DB
    save_outliers(db,
                  tibble::tibble(
                    file_id = unique(opp_stats_all$file_id),
                    flag = FLAG_OK
                  ),
                  overwrite = FALSE)
    # Save opp2 stats to DB
    opp2_stats_all <- dplyr::bind_rows(lapply(answer, function(item) item$opp2_stats))
    save_opp2_stats(db, opp2_stats_all)
  }

  deltat <- proc.time() - ptm
  message("Filtered ", nrow(plan), " files in ", deltat[["elapsed"]], " seconds")
}

# Filter one hour of EVT data.
#
# x is a dataframe created by create_full_filter_plan() that contains
# configuration data for filtering one hour of EVT data.
#
# y will be ignored, it is usually the group key (OPP file path) required by
# dplry::group_map but here we get the group key from the single unique value in
# x$window_opp_path.
#
# filter_params is a named list that can be used to lookup filter parameters by
# filter ID string.
filter_window_evt <- function(x, y, filter_params, max_particles_per_file = NULL,
                              max_opp_per_file = NULL) {
  plan <- x
  stopifnot(length(unique(plan$window_opp_path)) == 1)
  window_opp_path <- plan$window_opp_path[1]

  logtext <- paste0(lubridate::now("GMT"), " :: ", Sys.getpid(), " :: starting ", basename(window_opp_path))

  filtered <- dplyr::group_map(
    dplyr::group_by(plan, file_id),
    filter_3min_evt,
    filter_params,
    enforce_all_quantiles = TRUE,
    max_particles_per_file = max_particles_per_file,
    max_opp_per_file = max_opp_per_file,
    .keep=TRUE
  )
  # Combine all 3 minute OPPs into one new time-windowed OPP
  window_opp_df <- dplyr::bind_rows(lapply(filtered, function(item) item$opp))
  # Transform (unlog)
  window_opp_df <- transformData(window_opp_df)
  # Prepare opp stats dataframe
  opp_stats_df <- dplyr::bind_rows(lapply(filtered, function(item) item$opp_stats))
  # Prepare opp2 stats dataframe
  opp2_stats_df <- dplyr::bind_rows(lapply(filtered, function(item) item$opp2_stats))

  dir.create(dirname(window_opp_path), showWarnings=FALSE, recursive=TRUE)
  if (file.exists(window_opp_path)) {
    # Read old data for this window and remove data for files we just filtered
    old_opp_df <- arrow::read_parquet(window_opp_path) %>%
      dplyr::filter(!(file_id %in% unique(plan$file_id)))
    window_opp_df <- dplyr::bind_rows(
      window_opp_df,
      dplyr::anti_join(old_opp_df, window_opp_df, by="file_id")
    )
    # If there's no data left in this window after filtering, remove empty file
    if (nrow(window_opp_df) == 0) {
      file.remove(window_opp_path)
    }
    rm(old_opp_df)
    gc()
  }

  # Write data to new OPP parquet if not empty
  if (nrow(window_opp_df) > 0) {
    # Sort by ascending date
    window_opp_df <- window_opp_df %>% dplyr::arrange(.data[["date"]])
    # Use a temp file to avoid partial writes
    tmpname <- mktempname(dirname(window_opp_path), basename(window_opp_path))
    arrow::write_parquet(window_opp_df, tmpname)
    file.rename(tmpname, window_opp_path)
  }

  logtext <- paste0(
    logtext,
    "\n",
    paste0(lubridate::now("GMT"), " :: ", Sys.getpid(), " :: finished ", basename(window_opp_path))
  )
  message(logtext)
  return(list(
    opp_stats = opp_stats_df,
    opp2_stats = opp2_stats_df
  ))
}

filter_3min_evt <- function(x, y, filter_params, enforce_all_quantiles = TRUE,
                            max_particles_per_file = NULL, max_opp_per_file = NULL) {
  plan <- x
  stopifnot(nrow(plan) == 1)
  stopifnot(nrow(y) == 1)
  filter_id_ <- as.character(plan$filter_id[1])
  fp <- filter_params[[filter_id_]]
  opp2_flag <- FLAG_OPP2_OK
  opp2_msg <- ""
  max_check_row_count <- 0

  # Check if EVT file event count is above max_particles_per_file
  if (is.numeric(max_particles_per_file)) {
    result <- tryCatch({
      list(count = readSeaflow(plan$path[1], count.only=TRUE), msg = "", flag = FLAG_OPP2_OK)
    }, warning = function(err) {
      message(err$message)
      list(count = 0, msg = err$message, flag = FLAG_OPP2_EMPTY)
    }, error = function(err) {
      message(err$message)
      list(count = 0, msg = err$message, flag = FLAG_OPP2_EMPTY)
    })
    max_check_row_count <- result$count
    opp2_msg <- result$msg
    opp2_flag <- result$flag
    if (max_check_row_count > max_particles_per_file) {
      opp2_msg <- paste0("File has too many events (", max_check_row_count, " > ", max_particles_per_file, "): ", plan$path[1])
      message(opp2_msg)
      opp2_flag <- FLAG_OPP2_EVT_HIGH
    }
  }
  # Read full EVT file
  if (opp2_flag == FLAG_OPP2_OK) {
    result <- tryCatch({
      list(evt = readSeaflow(plan$path[1], transform = FALSE), msg = "", flag = FLAG_OPP2_OK)
    }, warning = function(err) {
      message(err$message)
      list(evt = data.frame(), msg = err$message, flag = FLAG_OPP2_EMPTY)
    }, error = function(err) {
      message(err$message)
      list(evt = data.frame(), msg = err$message, flag = FLAG_OPP2_EMPTY)
    })
    evt <- result$evt
    opp2_msg <- result$msg
    opp2_flag <- result$flag
  } else {
    evt <- data.frame()
    # Flag and msg set above
  }
  row_count <- nrow(evt)

  if (nrow(evt) > 0) {
    # Read the file OK, can continue with filtering
    # Keep a standard set of columns
    evt <- evt[, c("D1", "D2", "fsc_small", "pe", "chl_small")]
    noise <- identify_noise(evt)
    saturated <- identify_saturated(evt)
    noise_count <- sum(noise)
    saturated_count <- sum(saturated)
    evt_count <- sum(!noise)
    all_count <- row_count
  } else {
    # Something is wrong with file. opp2_flag should be > 0. Could be an error
    # on read or could have failed the max_particles_per_file check.
    stopifnot(opp2_flag > 0)  # just to be sure
    noise_count <- 0
    saturated_count <- 0
    evt_count <- 0
    all_count <- 0
    if (opp2_flag == FLAG_OPP2_EVT_HIGH) {
      # If we failed the max_particles_per_file check, we still want to record
      # the number of events in the file even though no further processing was
      # performed.
      all_count <- max_check_row_count
    }
  }

  # Filter EVT to OPP
  # Return empty tibble on warning or error
  opp <- tryCatch({
    tibble::as_tibble(filter_evt(evt, fp))
  }, warning = function(err) {
    message(err$message)
    return(tibble::tibble())
  }, error = function(err) {
    message(err$message)
    return(tibble::tibble())
  })

  gc()

  # Add metadata columns
  opp <- opp %>%
    dplyr::mutate(date = plan$date[1], .before = 1) %>%
    dplyr::mutate(file_id = as.factor(as.character(plan$file_id[1])), .after = date) %>%
    dplyr::mutate(filter_id = as.factor(filter_id_), .after = last_col())

  # Prep counts and ratios
  if (nrow(opp) > 0) {
    opp_counts <- sapply(
      QUANTILES,
      function(q) {
        sum(opp[[paste0("q", as.character(q))]])
      }
    )
    opp_evt_ratios <- opp_counts / evt_count
  } else {
    opp_counts <- c(0, 0, 0)
    opp_evt_ratios <- c(0.0, 0.0, 0.0)
  }

  if (!is.null(max_opp_per_file) && any(opp_counts > max_opp_per_file)) {
    # Protect against cases where a large fraction of EVT particles are OPP.
    # These cases may use more memory than expected and disrupt analysis
    # pipelines. As these results will never get used in downstream analysis it
    # is safe to discard them here.
    opp2_msg <- paste0("File has too many OPP (any(c(", paste(opp_counts, collapse = ", "), ") > ", max_opp_per_file, ")): ", plan$path[1])
    opp2_flag <- FLAG_OPP2_OPP_HIGH
    message(opp2_msg)
    opp <- head(opp, 0)
  }

  # Optionally only keep data if there are focused particles in all quantiles
  # Here we store the true counts and ratios in the opp table even though we
  # don't store the full particle data.
  if (enforce_all_quantiles && (opp2_flag == FLAG_OPP2_OK) && any(opp_counts == 0)) {
    opp2_flag <- FLAG_OPP2_EMPTY_QUANTILE
    opp2_msg <- paste0("At least one quantile has no OPP particles:", plan$path[1])
    opp <- head(opp, 0)
  }

  # Reset factors in case all rows were dropped
  opp <- opp %>%
    dplyr::mutate(
      file_id = forcats::fct_drop(file_id),
      filter_id = forcats::fct_drop(filter_id)
    )

  # Prepare data for opp table
  opp_stats <- tibble::tibble(
    date = plan$date[1],
    file_id = as.character(plan$file_id[1]),
    evt_count = evt_count,
    all_count = all_count,
    opp_count = opp_counts,
    opp_evt_ratio = opp_evt_ratios,
    quantile = QUANTILES,
    filter_id = filter_id_
  )
  # Prepare data for opp2 table
  opp2_stats <- tibble::tibble(
    file_id = as.character(plan$file_id[1]),
    all_count = all_count,
    evt_count = evt_count,
    opp_count = opp_counts[2],  # only keep 50% quantile for opp2 table
    opp_evt_ratio = opp_evt_ratios[2],  # only keep 50% quantile for opp2 table
    noise_count = noise_count,
    saturated_count = saturated_count,
    filter_id = filter_id_,
    message = opp2_msg,
    file_flag = opp2_flag
  )
  return(list(
    opp = opp,
    opp_stats = opp_stats,
    opp2_stats = opp2_stats
  ))
}

#' Estimate coordinates of the inflection point (location of 1µm beads).
#'
#' @param df Data frame containing log (untransformed) EVT data
#' @return 25th, 50th, 75th quantiles for D1, D2 and fsc values of presumed 1µm
#'   beads, labeled as quantiles "2.5", "50", "97.5".
#' @export
inflection_point <- function(df) {
  def_par <- par(no.readonly = TRUE) # save default, for resetting...
  par(mfrow = c(1,3), pty = "s")

  plot_cyt(df, "fsc_small", "pe")

  poly_beads <- splancs::getpoly(quiet = TRUE)
  points <- df[, c("fsc_small", "pe")]
  colnames(points) <- c("x", "y")
  b <- subset(df, splancs::inout(points, poly = poly_beads, bound = TRUE, quiet = TRUE))

  plot_cyt(b, "fsc_small", "D1")
  abline(h = 29000, lwd = 1, col = "red3")
  abline(v = 44500, lwd = 1, col = "red3")

  polyd1 <- splancs::getpoly(quiet=TRUE)
  points <- b[, c("fsc_small", "D1")]
  colnames(points) <- c("x", "y")
  opp.d1 <- subset(b,splancs::inout(points, poly = polyd1, bound = TRUE, quiet = TRUE))

  plot_cyt(b, "fsc_small", "D2")
  abline(h = 29000, lwd = 1, col = "red3")
  abline(v = 44500, lwd = 1, col = "red3")

  polyd2 <- splancs::getpoly(quiet=TRUE)
  points <- b[, c("fsc_small", "D2")]
  colnames(points) <- c("x", "y")
  opp.d2 <- subset(b, splancs::inout(points, poly = polyd2, bound = TRUE, quiet = TRUE))

  FSC <- round(summary(c(opp.d1$fsc_small, opp.d2$fsc_small)))
  D1 <- round(summary(opp.d1$D1))
  D2 <- round(summary(opp.d2$D2))

  inflection <- data.frame()
  for (quant in QUANTILES) {
    if (quant == 2.5) {
      i <- 2; j <- 5
    } else if (quant == 50.0) {
      i <- j <- 3
    } else if (quant == 97.5) {
      i <- 5; j <- 2
    }
    fsc <- as.vector(FSC[i])
    d1 <- as.vector(D1[j])
    d2 <- as.vector(D2[j])
    newrow <- data.frame(quantile = quant, fsc, d1, d2, stringsAsFactors = FALSE)
    inflection <- rbind(inflection, newrow)
  }
  par(def_par)

  return(inflection)
}

#' Construct filter parameters from 1µm bead coordinates
#'
#' @param inst Instrument serial
#' @param fsc 25th, 50, 75th quantiles of bead fsc_small
#' @param d1 25th, 50, 75th quantiles of bead D1
#' @param d2 25th, 50, 75th quantiles of bead D2
#' @param min_d1 Y-offset for small particle filter in D1
#' @param min_d2 Y-offset for small particle filter in D2
#' @param width Tolerance for D1 == D2 alignment test during filtering
#' @param slope_file CSV file of instrument-calibrated filtering slopes
#' @return Data frame of filtering parameters
#' @export
create_filter_params <- function(inst, fsc, d1, d2, min_d1, min_d2,
                                 width = 5000, slope_file = NULL) {
  # Rename to get correct dataframe headers
  beads.fsc.small <- as.numeric(fsc)
  beads.D1 <- as.numeric(d1)
  beads.D2 <- as.numeric(d2)
  min.D1 <- as.numeric(min_d1)
  min.D2 <- as.numeric(min_d2)

  width <- as.numeric(width)

  if (is.null(slope_file)) {
    slope_file <- "https://raw.githubusercontent.com/seaflow-uw/seaflow-virtualcore/master/1.bead_calibration/seaflow_filter_slopes.csv"
  }
  slopes <- read.csv(slope_file)

  filter.params <- data.frame()
  headers <- c(
    "quantile", "beads.fsc.small",
    "beads.D1", "beads.D2", "width",
    "notch.small.D1", "notch.small.D2",
    "notch.large.D1", "notch.large.D2",
    "offset.small.D1", "offset.small.D2",
    "offset.large.D1", "offset.large.D2"
  )
  for (quant in QUANTILES) {
    if (quant == 2.5) {
      suffix <- "_2.5"
      i <- 1
    } else if (quant == 97.5) {
      suffix <- "_97.5"
      i <- 3
    } else if (quant == 50.0) {
      suffix <- ""
      i <- 2
    }

    # Small particles
    offset.small.D1 <- min.D1
    offset.small.D2 <- min.D2
    notch.small.D1 <- round((beads.D1[i]-min.D1)/beads.fsc.small[i],3)
    notch.small.D2 <- round((beads.D2[i]-min.D2)/beads.fsc.small[i],3)

    # Large particles
    notch.large.D1 <- round(slopes[slopes$ins == inst, paste0('notch.large.D1', suffix)], 3)
    notch.large.D2 <- round(slopes[slopes$ins == inst, paste0('notch.large.D2', suffix)], 3)
    offset.large.D1 <- round(beads.D1[i] - notch.large.D1 * beads.fsc.small[i])
    offset.large.D2 <- round(beads.D2[i] - notch.large.D2 * beads.fsc.small[i])

    newrow <- data.frame(
      quant, beads.fsc.small[i],
      beads.D1[i], beads.D2[i], width,
      notch.small.D1, notch.small.D2,
      notch.large.D1, notch.large.D2,
      offset.small.D1, offset.small.D2,
      offset.large.D1, offset.large.D2,
      stringsAsFactors=FALSE
    )
    names(newrow) <- headers
    filter.params <- rbind(filter.params, newrow)
  }

  return(filter.params)
}