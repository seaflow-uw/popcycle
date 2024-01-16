# Gating and filter plan functions

#' Update the gating_plan or filter_plan table based on vct or opp entries
#'
#' @param db popcycle SQLite3 database file in which to save plan table
#' @param results_table opp or vct table
#' @param check Report changes to be made without saving to the database
#' @return Invisibly return the updated plan
#' @export
update_plan_table <- function(db, results_table, check = FALSE) {
  results_table <- tibble::as_tibble(results_table)

  if (nrow(results_table) == 0) {
    message("results table is empty, can't create gating_plan from results")
    return()
  }

  if ("gating_id" %in% colnames(results_table)) {
    id_col <- "gating_id"
  } else if ("filter_id" %in% colnames(results_table)) {
    id_col <- "filter_id"
  } else {
    stop("gating_id or filter_id must be a column in tbl")
  }

  if (id_col == "gating_id") {
    message("updating gating_plan based on vct results")
    message("received vct table with ", nrow(results_table), " rows")
    old_plan <- tryCatch(get_gating_plan_table(db), error={function(e) {NULL}})
    db_saver <- save_gating_plan
  } else {
    message("updating filter_plan based on opp results")
    message("received opp table with ", nrow(results_table), " rows")
    old_plan <- tryCatch(get_filter_plan_table(db), error={function(e) {NULL}})
    db_saver <- save_filter_plan
  }

  message("existing plan:")
  if (is.null(old_plan)) {
    old_plan_str <- "NULL"
  } else {
    old_plan_str <- paste(format(old_plan, n=1000), collapse="\n")
  }
  message(old_plan_str)

  starts <- plan_from_table(results_table)

  if (is.null(old_plan) || !isTRUE(dplyr::all_equal(starts, old_plan))) {
    message("existing plan will be replaced")
    message("new plan:")
    message(paste(format(starts, n=1000), collapse="\n"))

    if (!check) {
      make_popcycle_db(db)  # make sure db has the correct tables and views
      message("updated db schema for ", db)
      db_saver(db, as.data.frame(starts))
      message("plan table updated from results table")
    }
  } else {
    message("existing plan is already up to date")
  }
  invisible(starts)
}

#' Create a tibble of start dates for gating / filter IDs based vct / opp table entries.
#'
#' @param tbl VCT or OPP table as data.frame or tibble
#' @return tibble with character columns start_date and gating_id
#' @export
plan_from_table <- function(tbl) {
  if (nrow(tbl) == 0) {
    stop("table is empty")
  }
  if ("gating_id" %in% colnames(tbl)) {
    id_col <- "gating_id"
  } else if ("filter_id" %in% colnames(tbl)) {
    id_col <- "filter_id"
  } else {
    stop("gating_id or filter_id must be a column in table")
  }

  tbl <- tbl[order(tbl$date), ]
  # Run length encoding
  rle_result <- rle(tbl[[id_col]])
  # Indexes for end of each run of id_col
  end_idx <- cumsum(rle_result$lengths)
  # Indexes for start of each run of id_col
  start_idx <- c(1, end_idx + 1)[seq(1, length(end_idx))] # drop last index
  # Sanity check
  stopifnot(tbl[start_idx, ][[id_col]] == rle_result$values)
  starts <- tibble::tibble(
    start_date = tbl[start_idx, ]$date,
    temp_col = tbl[start_idx, ][[id_col]]
  )
  starts[[id_col]] <- starts$temp_col
  starts$temp_col <- NULL
  return(starts)
}

create_full_filter_plan <- function(file_ids_to_filter, db, evt_dir, opp_dir, filter_id = NULL) {
  # Normalize opp_dir to make sure all paths are comparable
  evt_dir <- normalizePath(evt_dir, mustWork=TRUE)
  # Get EVT files and dates to filter. If no files are provided, select all EVT
  # files present in SFL.
  df <- get_evt_files(evt_dir, db = db)
  if ((!is.null(file_ids_to_filter)) && length(file_ids_to_filter) > 0) {
    tmpdf <- tibble::tibble(file_id = clean_file_path(file_ids_to_filter))
    df <- df %>% dplyr::inner_join(tmpdf, df, by = "file_id")
  }

  # Add hours and hourly intervals
  df <- add_hours(df)
  # Add time-windowed OPP paths calculated from dates
  df <- add_opp_paths(df, opp_dir)
  # Add filter IDs based on filter_plan table or filter_id parameter
  df <- add_filter_ids(df, db, filter_id = filter_id)
  if (any(is.na(df$filter_id))) {
    stop("Some files are not covered by filter_plan date ranges")
  }
  # Filter down to only files with changed filter IDs
  opp <- get_opp_table(db, particles_in_all_quantiles = FALSE) %>%
    dplyr::select(file_id, filter_id)
  changed_file_ids <- dplyr::left_join(df, opp, by = "file_id") %>%
    dplyr::filter(is.na(filter_id.y) | (filter_id.x != filter_id.y)) %>%
    dplyr::pull(file_id) %>%
    unique()
  df <- df[df$file_id %in% changed_file_ids, ]

  return(df)
}

create_full_gating_plan <- function(file_ids_to_gate, db, opp_dir, vct_dir, gating_id = NULL) {
  # Normalize opp_dir to make sure all paths are comparable
  opp_dir <- normalizePath(opp_dir, mustWork=TRUE)
  # Get OPP files and dates to gate. If no files are provided, select all OPP
  # files with data in all quantiles.
  df <- get_opp_table(db, sfl_join = TRUE, particles_in_all_quantiles = TRUE) %>%
    dplyr::filter(quantile == 2.5) %>%  # select a single quantile, doesn't matter which one
    dplyr::select(date, file_id)
  if (!is.null(file_ids_to_gate) && length(file_ids_to_gate) > 0) {
    tmpdf <- tibble::tibble(file_id = clean_file_path(file_ids_to_gate))
    df <- df %>% dplyr::inner_join(tmpdf, df, by = "file_id")
  }

  # Add hours and hourly intervals
  df <- add_hours(df)
  # Add time-windowed OPP and VCT file paths and intervals based on row date
  df <- add_opp_paths(df, opp_dir)
  df <- add_vct_paths(df, vct_dir)
  # Add gating IDs based on row file_id or date
  df <- add_gating_ids(df, db, gating_id = gating_id)
  if (any(is.na(df$gating_id))) {
    stop("Some files are not covered by gating_plan date ranges")
  }
  # Filter down to only files with changed gating IDs
  vct <- get_vct_table(db) %>%
    dplyr::select(file_id, gating_id)
  changed_file_ids <- dplyr::left_join(df, vct, by = "file_id") %>%
    dplyr::filter(is.na(gating_id.y) | (gating_id.x != gating_id.y)) %>%
    dplyr::pull(file_id) %>%
    unique()
  df <- df[df$file_id %in% changed_file_ids, ]
  # Add filter_id and per-quantile fsc bead pos from filter parameters
  df <- add_filter_params(df, db)
  # Add instrument serial number
  df <- add_instrument_serial(df, db)

  return(df)
}

add_gating_ids <- function(df, db, gating_id = NULL) {
  # Add a "gating_id" column to df
  if (!is.null(gating_id)) {
    df$gating_id <- gating_id
  } else {
    # Consult the gating_plan table to add gating_id by date
    gating_plan <- get_gating_plan_table(db)
    if (nrow(gating_plan) == 0) {
      stop("gating_plan table is empty")
    }
    if (nrow(gating_plan) > 1) {
      for (i in seq(1, nrow(gating_plan) - 1)) {
        inrange <- (df$date >= gating_plan[i, ][["start_date"]]) & (df$date < gating_plan[i + 1, ][["start_date"]])
        df[inrange, "gating_id"] <- gating_plan[i, ][["gating_id"]]
      }
    }
    i <- nrow(gating_plan)
    inrange <- df$date >= gating_plan[i, ][["start_date"]]
    df[inrange, "gating_id"] <- gating_plan[i, ][["gating_id"]]
  }
  return(df)
}

add_filter_params <- function(df, db) {
  # Add "filter_id", "beads_fsc_2.5", "beads_fsc_50", "beads_fsc_97.5" columns
  filter_plan <- get_filter_plan_table(db)
  if (nrow(filter_plan) == 0) {
    stop("filter_plan table is empty")
  }
  if (nrow(filter_plan) > 1) {
    for (i in seq(1, nrow(filter_plan) - 1)) {
      inrange <- (df$date >= filter_plan[i, ][["start_date"]]) & (df$date < filter_plan[i + 1, ][["start_date"]])
      df[inrange, "filter_id"] <- filter_plan[i, ][["filter_id"]]
      fp <- get_filter_params_by_id(db, filter_plan[i, ][["filter_id"]])
      df[inrange, "beads_fsc_2.5"] <- transformData(data.frame(fsc=fp[fp$quantile == 2.5, "beads.fsc.small"]), columns = c("fsc"))$fsc[1]
      df[inrange, "beads_fsc_50"] <- transformData(data.frame(fsc=fp[fp$quantile == 50, "beads.fsc.small"]), columns = c("fsc"))$fsc[1]
      df[inrange, "beads_fsc_97.5"] <- transformData(data.frame(fsc=fp[fp$quantile == 97.5, "beads.fsc.small"]), columns = c("fsc"))$fsc[1]
    }
  }
  i <- nrow(filter_plan)
  inrange <- df$date >= filter_plan[i, ][["start_date"]]
  fp <- get_filter_params_by_id(db, filter_plan[i, ][["filter_id"]])
  df[inrange, "filter_id"] <- filter_plan[i, ][["filter_id"]]
  df[inrange, "beads_fsc_2.5"] <- transformData(data.frame(fsc=fp[fp$quantile == 2.5, "beads.fsc.small"]), columns = c("fsc"))$fsc[1]
  df[inrange, "beads_fsc_50"] <- transformData(data.frame(fsc=fp[fp$quantile == 50, "beads.fsc.small"]), columns = c("fsc"))$fsc[1]
  df[inrange, "beads_fsc_97.5"] <- transformData(data.frame(fsc=fp[fp$quantile == 97.5, "beads.fsc.small"]), columns = c("fsc"))$fsc[1]

  return(df)
}

add_instrument_serial <- function(df, db) {
  df$inst <- get_inst(db)
  return(df)
}

add_hours <- function(df) {
  # Add hour date (hour) and hourly interval (interval)
  interval_size <- lubridate::hours(1) - lubridate::milliseconds(1)
  df <- df %>%
    dplyr::mutate(hour = lubridate::floor_date(date, "hour")) %>%
    dplyr::mutate(interval = lubridate::interval(hour, hour + interval_size))
  return(df)
}

add_opp_paths <- function(df, opp_dir) {
  # Add hour date and hourly opp path
  df <- df %>%
    dplyr::mutate(hour = lubridate::floor_date(date, "hour")) %>%
    dplyr::mutate(window_opp_path = to_filename_date_str(hour)) %>%
    dplyr::mutate(window_opp_path = stringr::str_c(window_opp_path, ".1H.opp.parquet")) %>%
    dplyr::mutate(window_opp_path = file.path(opp_dir, window_opp_path))
  return(df)
}

add_vct_paths <- function(df, vct_dir) {
  # Add hour date and hourly vct path
  df <- df %>%
    dplyr::mutate(hour = lubridate::floor_date(date, "hour")) %>%
    dplyr::mutate(window_vct_path = to_filename_date_str(hour)) %>%
    dplyr::mutate(window_vct_path = stringr::str_c(window_vct_path, ".1H.vct.parquet")) %>%
    dplyr::mutate(window_vct_path = file.path(vct_dir, window_vct_path))
  return(df)
}

add_filter_ids <- function(df, db, filter_id = NULL) {
  # Add a "filter_id" column to df
  if (!is.null(filter_id)) {
    df$filter_id <- filter_id
  } else {
    # Consult the filter_plan table to add filter_id by date
    filter_plan <- get_filter_plan_table(db)
    if (nrow(filter_plan) == 0) {
      stop("filter_plan table is empty")
    }
    if (nrow(filter_plan) > 1) {
      for (i in seq(1, nrow(filter_plan) - 1)) {
        inrange <- (df$date >= filter_plan[i, ][["start_date"]]) & (df$date < filter_plan[i + 1, ][["start_date"]])
        df[inrange, "filter_id"] <- filter_plan[i, ][["filter_id"]]
      }
    }
    i <- nrow(filter_plan)
    inrange <- df$date >= filter_plan[i, ][["start_date"]]
    df[inrange, "filter_id"] <- filter_plan[i, ][["filter_id"]]
  }
  return(df)
}