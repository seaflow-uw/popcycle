#' Find EVT files with a recursive search down a directory tree.
#'
#' @param evt_dir Directory containing EVT files.
#' @param db SQLite3 popcycle database file. If present, inner join to sfl table
#'   and add a date column.
#' @return Tibble of EVT file paths, file IDs, and optionally dates: ("path",
#'   "file_id", "date").
#' @export
get_evt_files <- function(evt_dir, db = NULL) {
  file_list <- sort(list.files(evt_dir, recursive = T, full.names = TRUE))

  # regexp to match both types of EVT files
  #   - 37.evt (old style)
  #   - 2014-05-15T17-07-08+0000 or 2014-07-04T00-03-02+00-00 (new style)
  # In the new style the final timezone offset may not always be UTC (00-00)
  # so be sure to correctly parse it in all code.
  regexp <- "/?[0-9]+\\.evt(?:\\.gz|\\.parquet)?$|/?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}[+-][0-9]{2}-?[0-9]{2}(?:\\.gz|\\.parquet)?$"
  file_list <- file_list[grep(regexp, file_list)]

  clean_file_list <- clean_file_path(file_list)
  files <- tibble::tibble(path = file_list, file_id = as.character(clean_file_list))

  if (!is.null(db)) {
    sfl <- get_sfl_table(db) %>%
      dplyr::select(date, file_id = file)
    files <- dplyr::inner_join(files, sfl, by = "file_id") %>%
      dplyr::relocate(date)
  }

  return(files)
}

#' Clean a file path.
#'
#' Convert an EVT/OPP/VCT file path to a form suitable for storage in the SQLite
#' db. Any ".gz", ".parquet", ".opp", ".vct" extensions will be removed.
#'
#' @param paths Character vector of file paths to clean.
#' @return Modified file path as julian_day/EVT_file_name.
#' @export
clean_file_path <- function(paths) {
  cleaner <- function(fpath) {
    # Clean up any places with multiple "/"s
    fpath <- gsub("/+", "/", fpath)

    # Check for julian day directory
    parts <- unlist(strsplit(fpath, "/"))
    if (length(parts) < 2) {
      stop(paste0("file path ", fpath, " must contain a julian day directory"))
    }

    file.name <- parts[length(parts)]
    julian.day <- parts[length(parts)-1]

    julian.regexp <- "^[0-9]{4}_[0-9]+$"
    if (length(grep(julian.regexp, julian.day)) != 1) {
      stop(paste0("Julian day directory does not match pattern YYYY_day in ", fpath))
    }

    # Get rid of any .gz extension
    if (nchar(file.name) >= 3) {
      if (substr(file.name, nchar(file.name) - 2, nchar(file.name)) == ".gz") {
        file.name <- substr(file.name, 1, nchar(file.name) - 3)
      }
    }

    # Get rid of any .parquet extension
    if (nchar(file.name) >= 8) {
      if (substr(file.name, nchar(file.name) - 7, nchar(file.name)) == ".parquet") {
        file.name <- substr(file.name, 1, nchar(file.name) - 8)
      }
    }

    # Get rid of any .opp extension
    if (nchar(file.name) >= 4) {
      if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) == ".opp") {
        file.name <- substr(file.name, 1, nchar(file.name) - 4)
      }
    }

    # Get rid of any .vct extension
    if (nchar(file.name) >= 4) {
      if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) == ".vct") {
        file.name <- substr(file.name, 1, nchar(file.name) - 4)
      }
    }
    return(paste(julian.day, file.name, sep="/"))
  }
  cleaned <- sapply(paths, cleaner)
  names(cleaned) <- NULL
  return(cleaned)
}

#' Get EVT data frame by file.
#'
#' @param evt.dir EVT file directory.
#' @param file_id File ID with julian day directory. Can be a single
#'   file name or vector of file names.
#' @param transform Linearize EVT data. Default is FALSE.
#' @return Data frame of EVT data for all files in file.name.
#' @export
get_evt_by_file <- function(evt_dir, file_id, transform = FALSE) {
  evt_files <- clean_file_path(file_id)
  evt_reader <- function(f) {
    return(readSeaflow(file.path(evt_dir, f), transform = transform))
  }
  evts <- lapply(evt_files, evt_reader)
  if (length(evts) > 1) {
    return(dplyr::bind_rows(evts))
  }
  return(evts[[1]])
}

#' Get tibble of filtered particles selecting by file IDs.
#'
#' @param db SQLite3 database file path.
#' @param opp_dir OPP file directory.
#' @param file_ids Vector of file IDs with julian day directory.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of filtered particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_opp_by_file <- function(db, opp_dir, file_ids, outliers = TRUE, col_select = NULL) {
  return(get_processed_particles(db, opp_dir, "opp", file_ids = file_ids,
                                 outliers = outliers, col_select = col_select))
}

#' Get tibble of filtered particles selecting by date.
#'
#' Datetime string formats should be in the form "2020-07-31 10:00".
#'
#' @param db SQLite3 database file path.
#' @param opp_dir OPP file directory.
#' @param start_date Start date, inclusive.
#' @param end_date End date, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of filtered particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_opp_by_date <- function(db, opp_dir, start_date, end_date, outliers = TRUE,
                            col_select = NULL) {
  return(get_processed_particles(db, opp_dir, "opp", start_date = start_date,
                                 end_date = end_date, outliers = outliers,
                                 col_select = col_select))
}

#' Get tibble of classified particles selecting by file IDs.
#'
#' @param db SQLite3 database file path.
#' @param vct_dir VCT file directory.
#' @param file_ids Vector of file IDs with julian day directory.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of classified particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_vct_by_file <- function(db, vct_dir, file_ids, outliers = TRUE, col_select = NULL) {
  return(get_processed_particles(db, vct_dir, "vct", file_ids = file_ids,
                                 outliers = outliers, col_select = col_select))
}

#' Get tibble of classified particles selecting by date.
#'
#' Datetime string formats should be in the form "2020-07-31 10:00".
#'
#' @param db SQLite3 database file path.
#' @param vct_dir VCT file directory.
#' @param start_date Start date, inclusive.
#' @param end_date End date, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of classified particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_vct_by_date <- function(db, vct_dir, start_date, end_date, outliers = TRUE,
                            col_select = NULL) {
  return(get_processed_particles(db, vct_dir, "vct", start_date = start_date,
                                 end_date = end_date, outliers = outliers,
                                 col_select = col_select))
}

#' Get a tibble of OPP or VCT particles
#'
#' @param db SQLite3 database file path.
#' @param data_dir Directory with OPP or VCT parquet files.
#' @param data_type "opp" or "vct".
#' @param file_ids Vector of file IDs with julian day directory.
#' @param start_date Start date, inclusive.
#' @param end_date End date, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a charactr vector. date and file_id are always added to to this vector. By
#'   default all columns are returned.
#' @return Tibble of OPP or VCT particles. If no data is found a tibble with
#'   zero rows will be returned.
get_processed_particles <- function(db, data_dir, data_type, file_ids = NULL,
                                    start_date = NULL, end_date = NULL,
                                    outliers = TRUE, col_select = NULL) {
  if (!is.null(start_date) && !("POSIXct" %in% class(start_date))) {
    stop("start_date must be a POSIXct date value")
  }
  if (!is.null(end_date) && !("POSIXct" %in% class(end_date))) {
    stop("end_date must be a POSIXct date value")
  }

  if (data_type == "opp") {
    table_getter <- get_opp_table
    path_adder <- add_opp_paths
    window_path_col <- "window_opp_path"
  } else if (data_type == "vct") {
    table_getter <- get_vct_table
    path_adder <- add_vct_paths
    window_path_col <- "window_vct_path"
  } else {
    stop("data_type must be 'opp' or 'vct'")
  }

  # Clean up parameters
  if (!is.null(file_ids)) {
    file_ids <- clean_file_path(file_ids)
  }
  # Alway make sure date and file_id are retrieved
  if (!is.null(col_select)) {
    col_select <- c("date", "file_id", col_select[!(col_select %in% c("date", "file_id"))])
  }
  # Make sure predicted and discovered hourly Parquet paths are comparable
  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  # Build file ID list
  to_get <- table_getter(db) %>%
    dplyr::filter(quantile == 50) %>% # only need one quantile to list files
    dplyr::select(date, file_id, flag)
  if (outliers) {
    to_get <- to_get %>% dplyr::filter(flag == FLAG_OK)
  }
  if (!is.null(file_ids)) {
    to_get <- to_get %>% dplyr::filter(file_id %in% file_ids)
  }
  if (!is.null(start_date)) {
    to_get <- to_get %>% dplyr::filter(date >= start_date)
  }
  if (!is.null(end_date)) {
    to_get <- to_get %>% dplyr::filter(date <= end_date)
  }

  if (nrow(to_get) == 0) {
    # No data found
    warning("no data found")
    return(tibble::tibble())
  }

  # Get hourly parquet files on disk
  hourly_files <- list.files(data_dir, pattern = paste0("*.", data_type, ".parquet"), full.names = TRUE)

  # Predict hourly parquet paths for each file, filter down to those on disk
  to_get <- path_adder(to_get, data_dir) %>%
    dplyr::filter(.data[[window_path_col]] %in% hourly_files) %>%
    dplyr::arrange(date)

  if (nrow(to_get) == 0) {
    # No data found
    warning("no corresponding parquet files found")
    return(tibble::tibble())
  }

  df <- to_get %>%
    dplyr::group_by(across(all_of(window_path_col))) %>%
    dplyr::group_map(function(rows, key) {
      arrow::read_parquet(key[[window_path_col]], col_select = {{ col_select }}) %>%
        dplyr::filter(file_id %in% rows$file_id)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_if(is.factor, forcats::fct_drop)

  # Check for file IDs not found in parquet files
  not_found <- !(to_get$file_id %in% df$file_id)
  if (sum(not_found) > 0) {
    warning("some file IDs not found:\n", paste(to_get$file_id[not_found], collapse = "\n"))
  }

  return(df)
}

#' Read an OPP/VCT parquet file, only grabbing columns needed to analyze one quantile.
#'
#' @param filepath Path of parquet file
#' @param quantile Quantile to select, e.g. 2.5
#' @param cols Columns to select. If a column has a refractive index suffix and/or
#'   a quantile suffix in the parquet file column name, don't include it here.
#'   For example, use "pop", not "pop_q2.5". Use "diam", not "diam_lwr_q2.5". The
#'   appropriate quantile boolean column will be added automatically if not present.
#'   If not a character vector, all columns will be returned.
#' @param refracs Dataframe of refractive indices to use for each population. Column names
#'   should be population names used in classification, values should be mid, lwr, upr.
#'   If refracs is supplied but "pop" is not in the cols parameter, it will be added automatically.
#' @export
read_parquet_one_quantile <- function(filepath, quantile, cols = NULL, refracs = NULL) {
  qstr <- paste0("q", quantile)
  qsuffix <- paste0("_", qstr)
  if (!is.character(cols)) {
    need_all_cols <- TRUE
  } else {
    need_all_cols <- FALSE
  }

  if (!is.null(refracs)) {
    if (nrow(refracs) != 1) {
      stop("refracs should only contain one row")
    }
    if ("cruise" %in% names(refracs)) {
      refracs <- refracs %>% dplyr::select(-c(cruise)) # sometimes left in accidentally
    }
    if (!all(refracs[1, ] %in% c("lwr", "mid", "upr"))) {
      stop("invalid refraction index label in: '", paste(refracs, collapse = " "), "'")
    }
    refracs_needed <- refracs %>%
      unlist() %>%
      unique()
    # Need to grab pop to apply per-pop refractive index
    if (!need_all_cols && !("pop" %in% cols)) {
      cols <- c(cols, "pop")
    }
  } else {
    # Get all refractive indexes
    refracs_needed <- c("lwr", "mid", "upr")
  }

  # Build a vector of column names to select
  if (need_all_cols) {
    df <- arrow::read_parquet(filepath)
  } else {
    cols_needed <- c(qstr)
    for (col in cols) {
      if ((col == "pop")) {
        cols_needed <- c(cols_needed, paste0("pop", qsuffix))
      } else if (col == "diam") {
        for (refrac_alias in refracs_needed) {
          cols_needed <- c(cols_needed, paste0("diam_", refrac_alias, qsuffix))
        }
      } else if (col == "Qc") {
        for (refrac_alias in refracs_needed) {
          cols_needed <- c(cols_needed, paste0("Qc_", refrac_alias, qsuffix))
        }
      } else if (col != qstr) {
        cols_needed <- c(cols_needed, col)
      }
    }

    df <- arrow::read_parquet(filepath, col_select = c(any_of(cols_needed)))
  }

  df <- df %>%
    dplyr::filter(get(qstr)) %>%                                       # select one quantile of data
    dplyr::select(-c(any_of(c("q2.5", "q50", "q97.5")))) %>%           # remove any quantile boolean columns
    dplyr::rename_with(                                                # remove selected quantile suffix from columns
      function(x) { stringr::str_replace(x, paste0(qsuffix, "$"), "") },
      ends_with(qsuffix)
    ) %>%
    dplyr::select(
      -(ends_with("_q2.5") | ends_with("_q50") | ends_with("_q97.5"))  # remove any remaining quantile columns
    )

  # Apply population specific refractive indexes
  if (!is.null(refracs) && ("pop" %in% names(df))) {
    for (popname in names(refracs)) {
      refrac_alias <- refracs[[1, popname]]
      pop_idx <- df$pop == popname
      if (need_all_cols || "diam" %in% cols) {
        df[pop_idx, "diam"] <- df[pop_idx, paste0("diam_", refrac_alias)]
      }
      if (need_all_cols || "Qc" %in% cols) {
        df[pop_idx, "Qc"] <- df[pop_idx, paste0("Qc_", refrac_alias)]
      }
    }
    # Make sure pop is last
    df <- df %>% dplyr::relocate(pop, .after = last_col())
    if ("diam" %in% names(df)) {
      if (any(is.na(df$diam))) {
        stop("missing refractive index for at least one population")
      }
    }
    if ("Qc" %in% names(df)) {
      if (any(is.na(df$Qc))) {
        stop("missing refractive index for at least one population")
      }
    }
    for (refrac_alias in refracs_needed) {
      df[, endsWith(names(df), paste0("_", refrac_alias))] <- NULL
    }
  }
  return(df)
}

#' Create an empty EVT data frame
#'
#' @return EVT data frame with no rows
empty_evt <- function() {
  df <- data.frame(matrix(ncol = length(EVT.HEADER), nrow = 0))
  colnames(df) <- EVT.HEADER
  return(df)
}

#' Create an empty OPP data frame
#'
#' @return OPP data frame with no rows
empty_opp <- function() {
  return(empty_evt())
}

#' Read Mie theory calibration file
#'
#' @return A dataframe of Mie theory conversion values
#' @export
read_mie_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("scatter", paste0("calibrated-mie.csv"),package="popcycle")
  }
  return(read.csv(path))
}

#' Read Abundance calibration file
#'
#' @return A dataframe of regression values (cruise=cruise.name, pop=prochloro, a=slope, b=intercept)
#' @export
read_calib_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("abundance", paste0("abundance-calibration.csv"),package="popcycle")
  }
  return(read.csv(path))
}

#' Read table of indexes of refraction for cruise and population combinations
#'
#' @return A dataframe with columns for cruise and each population's index of refraction alias
#' @export
read_refraction_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("scatter", paste0("RefracIndices_percruise.csv"), package="popcycle")
  }
  return(read.csv(path))
}

#' Read table of PAR calibration values
#'
#' @return A dataframe with columns for cruise and PAR calibration factor
#' @export
read_par_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("par", paste0("par-calibration.csv"), package="popcycle")
  }
  return(read.csv(path))
}

#' Read table of reference filter parameters for a single instrument
#'
#' @param inst Instrument serial
#' @return A dataframe of filter parameters
#' @export
read_reference_filter_params <- function(inst=NULL) {
  if (is.null(inst)) {
    stop("inst must be specified")
  }
  system_path <- paste0("reference_filter_params_", inst, ".csv")
  path <- system.file("filter", system_path, package="popcycle")
  if (path == "") {
    stop("could not find system file for package 'popcycle': ", system_path)
  }
  return(read.csv(path))
}

#' Read SFL tab-delimited file
#' @param path Path to validated tab-delimited SFL file
#' @return A tibble with SFL data
#' @export
read_sfl_tsv <- function(path) {
  sfl_data <- readr::read_delim(
    path,
    delim = "\t",
    col_types = list(
      FILE = readr::col_character(),
      DATE = readr::col_character(),
      `FILE DURATION` = readr::col_double(),
      LAT = readr::col_double(),
      LON = readr::col_double(),
      CONDUCTIVITY = readr::col_double(),
      SALINITY = readr::col_double(),
      `OCEAN TEMP` = readr::col_double(),
      PAR = readr::col_double(),
      `BULK RED` = readr::col_double(),
      `STREAM PRESSURE` = readr::col_double(),
      `EVENT RATE` = readr::col_double()
    )
  ) %>% rename(
    file = FILE,
    date = DATE,
    file_duration = `FILE DURATION`,
    lat = LAT,
    lon = LON,
    conductivity = CONDUCTIVITY,
    salinity = SALINITY,
    ocean_tmp = `OCEAN TEMP`,  # not a typo
    par = PAR,
    bulk_red = `BULK RED`,
    stream_pressure = `STREAM PRESSURE`,
    event_rate = `EVENT RATE`
  )
  return(sfl_data)
}


#' Clean the stat table by selecting the best refractive index from the table (RefracIndices_percruise.csv),
#' keep only the clean data (flag ==0 ) and remove percentile column
#'
#' @param db SQLite3 database file path.
#' @return A cleaned stat table with corrected refractive index
#' @export
get_clean_stat_table <- function(db){

  cruisename <- get_cruise(db)
  print(cruisename)
  stat <- tibble::as_tibble(get_stat_table(db))
  stat <- stat_calibration(stat, cruisename)

  ### Select the appropriate refractive index for each population
  refracs_cruises <- read_refraction_csv()
  refracs <- refracs_cruises %>% dplyr::filter(cruise == cruisename)
  print("per-population refractive indices")
  print(refracs)
  if (nrow(refracs) == 0) {
    stop("no entry found for ", cruisename, " in refrative index table")
  }

  # select the appropriate data
  clean <- tibble::tibble()

  for(phyto in c("prochloro","synecho","picoeuk","croco")){
    n <- refracs[phyto]

    p <- stat %>%
          dplyr::filter(flag == 0, quantile == 50, pop == phyto) %>%
          dplyr::select(time,lat,lon, pop, abundance, contains(c(paste0(n,"_med"),paste0(n,"_mean")))) %>%
          dplyr::rename(diam = contains(paste0("diam_",n,"_med")), Qc = contains(paste0("Qc_",n,"_med"))) %>%
          dplyr::mutate(biomass = abundance * Qc) %>%
          dplyr::select(!ends_with("mean"))
    clean <- clean %>% dplyr::bind_rows(p)
  }

  return(clean)

}

#' Create a temporary file name
mktempname <- function(root_dir, suffix) {
  rand8char <- stringr::str_sub(uuid::UUIDgenerate(), 1, 8)
  timestamp <- lubridate::format_ISO8601(lubridate::now())
  return(file.path(root_dir, glue::glue("._{rand8char}_{timestamp}_{suffix}")))
}