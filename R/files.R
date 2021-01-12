#' Find EVT files with a recursive search down a directory tree.
#'
#' @param evt.dir Directory containing EVT files.
#' @return Vector of EVT files with julian day directory.
#' @examples
#' \dontrun{
#' evt.files <- get.evt.files(evt.dir)
#' }
#' @export
get.evt.files <- function(evt.dir) {
  file.list <- list.files(evt.dir, recursive=T)
  if (length(file.list) == 0) {
    print(paste("no evt files found in", evt.dir))
    return(file.list)
  }
  # regexp to match both types of EVT files
  #   - 37.evt (old style)
  #   - 2014-05-15T17-07-08+0000 or 2014-07-04T00-03-02+00-00 (new style)
  # In the new style the final timezone offset may not always be UTC (00-00)
  # so be sure to correctly parse it in all code.
  regexp <- "/?[0-9]+\\.evt(\\.gz)?$|/?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}[+-][0-9]{2}-?[0-9]{2}(\\.gz)?$"
  id <- grep(regexp,file.list)
  file.list <- file.list[id]
  # Only keep files that look like doy/filename. i.e. don't go deeper in the
  # directory structure, filter out sub/dir/doy/filename
  file.list <- file.list[lapply(stringr::str_split(file.list, "/"), length) <= 2]
  #print(paste(length(file.list), "evt files found"))
  return(sort(unlist(lapply(file.list, clean.file.path))))
}

#' Find the most recent EVT file.
#'
#' @param evt.dir Directory containing EVT files.
#' @return Most recent EVT file with julian day directory.
#' @examples
#' \dontrun{
#' evt.file <- get.latest.evt(evt.dir)
#' }
#' @export
get.latest.evt <- function(evt.dir) {
  file.list <- get.evt.files(evt.dir)
  n <- length(file.list)
  if (n == 0) {
    return(NA)
  }
  return(clean.file.path(file.list[n]))
}


#' Clean a file path.
#'
#' Convert an EVT/OPP/VCT file path to a form suitable for storage in the SQLite
#' db. Any ".gz", ".opp", ".vct" extensions will be removed.
#'
#' @param fpath File path to clean.
#' @return Modified file path as julian_day/EVT_file_name.
#' @examples
#' \dontrun{
#' fpath <- clean.file.path("foo/2014_185/2014-07-04T00-00-02+00-00.opp.gz")
#' }
#' @export
clean.file.path <- function(fpath) {
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

#' file_transfer
#'
#' @return None
#' @export
file_transfer <- function(evt.dir, instrument.dir){

  last.evt <- get.latest.evt(evt.dir)
  file.list <- list.files(instrument.dir, recursive=T)
  # Only keep files that look like doy/filename. i.e. don't go deeper in the
  # directory structure, filter out sub/dir/doy/filename
  file.list <- file.list[lapply(stringr::str_split(file.list, "/"), length) <= 2]
  sfl.list <- file.list[grepl('.sfl', file.list)]
  # Now get only EVT files, no SFL or anything else
  file.list <- select_files_in(file.list, get.evt.files(instrument.dir))
  file.list <- file.list[-length(file.list)] # remove the last EVT file (opened file)

  id <- match(last.evt, file.list)

  if (length(file.list) > 0) {
    if (is.na(id)) {
      day <- unique(dirname(file.list))
        for(d in day) system(paste0("mkdir ",evt.dir,"/",d))
      print(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list))
      system(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list, collapse=";"))
      system(paste0("scp ",instrument.dir,"/",sfl.list," ", evt.dir,"/",sfl.list, collapse=";"))
    }
    else{
      file.list <- file.list[id:length(file.list)]
      day <- unique(dirname(file.list))
        for(d in day) system(paste0("mkdir ",evt.dir,"/",d))
      print(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list))
      system(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list, collapse=";"))
      system(paste0("scp ",instrument.dir,"/",sfl.list," ", evt.dir,"/",sfl.list, collapse=";"))
    }
  }
}

#' Select SeaFlow files based on presence in a second vector
#'
#' Only keep files in input vector if they are present in vector of files to
#' keep. Match based on the canonical SeaFlow file ID.
#'
#' @param files Vector of files to pull from
#' @param keep Vector of files to keep in <files>
#' @return Subset of <files> that are in <keep> based on SeaFlow file ID
#' @examples
#' \dontrun{
#' chosen <- select_files_in(files, keep)
#' }
#' @export
select_files_in <- function(files, keep) {
  # Convert to canonical SeaFlow file ID
  files_clean <- unlist(lapply(files, clean.file.path))
  keep_clean <- unlist(lapply(keep, clean.file.path))
  return(files[files_clean %in% keep_clean])
}

#' Get EVT data frame by file.
#'
#' @param evt.dir EVT file directory.
#' @param file.name File name with julian day directory. Can be a single
#'   file name or vector of file names.
#' @param transform Linearize EVT data. Default is FALSE.
#' @return Data frame of EVT data for all files in file.name.
#' @examples
#' \dontrun{
#' evt <- get.evt.by.file(evt.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.evt.by.file <- function(evt.dir, file.name, transform=FALSE) {
  evt_files <- unlist(lapply(file.name, clean.file.path))
  evt_reader <- function(f) {
    return(readSeaflow(file.path(evt.dir, f), transform=transform))
  }
  evts <- lapply(evt_files, evt_reader)
  if (length(evts) > 1) {
    return(dplyr::bind_rows(evts))
  }
  return(evts[[1]])
}

#' Get a tibble of filtered particles by file and quantile
#'
#' @param db SQLite3 database file path.
#' @param opp.dir OPP file directory.
#' @param file.name File name with julian day directory.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   tidy-selection. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of filtered particles for one file. If no data is found a
#'   tibble with zero rows will be returned.
#' @examples
#' \dontrun{
#' opp <- get.opp.by.file(opp.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.opp.by.file <- function(db, opp.dir, file.name, col_select=NULL) {
  file.name <- clean.file.path(file.name)
  col_select <- rlang::enquo(col_select)
  if (rlang::quo_is_null(col_select)) {
    col_select <- NULL
  } else {
    # Alway enforce dates and file_ids as the first columns
    col_select <- rlang::expr(c(date, file_id, {{ col_select }}))
  }
  opp_stats <- tibble::as_tibble(get.opp.stats.by.file(db, file.name))
  opp_stats <- dplyr::filter(opp_stats, quantile == 50)  # only need files and dates for one quantile
  if (nrow(opp_stats) == 0) {
    # No data found
    return(tibble::tibble())
  }

  # Only keep file_id and date as datetime object
  date_df <- dplyr::select(
    dplyr::mutate(opp_stats, file_id=file, date=lubridate::ymd_hms(date)),
    file_id,
    date
  )
  opp_files <- list.files(opp.dir, pattern=paste0("*.opp.parquet"), full.names=TRUE)
  # TODO this will throw an error if something in date_df isn't in opp_files
  # do something to handle it
  window_df <- add_window_paths(date_df, opp_files)  # associate files with parquet files
  # TODO check for no match
  opp_path <- window_df$window_path[1]
  opp_df <- dplyr::filter(
    tibble::as_tibble(arrow::read_parquet(opp_path, col_select={{ col_select }})),
    file_id == file.name
  )
  # Remove unused levels in factors
  opp_df <- dplyr::mutate_if(opp_df, is.factor, forcats::fct_drop)
  return(opp_df)
}

#' Get data frame of filtered particles selecting by date.
#'
#' Datetime string formats should be in the form "2020-07-31 10:00".
#'
#' @param db SQLite3 database file path.
#' @param opp.dir OPP file directory.
#' @param start.date Start date string, inclusive.
#' @param end.date End date string, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   tidy-selection. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Data frame of filtered particles. If no data is found a data frame
#'   with zero rows will be returned.
#' @examples
#' \dontrun{
#' opp <- get.opp.by.date(db, opp.dir, "2020-07-31 10:00", "2020-07-31 12:00"
#'                        col_select=c("fsc_small"))
#' }
#' @export
get.opp.by.date <- function(db, opp.dir, start.date, end.date, outliers=TRUE,
                            col_select=NULL) {
  col_select <- rlang::enquo(col_select)
  if (rlang::quo_is_null(col_select)) {
    col_select <- NULL
  } else {
    # Alway enforce dates and file_ids as the first columns
    col_select <- rlang::expr(c(date, file_id, {{ col_select }}))
  }

  opp_stats <- tibble::as_tibble(get.opp.stats.by.date(
    db, start.date=start.date, end.date=end.date, outliers=outliers
  ))
  opp_stats <- dplyr::filter(opp_stats, quantile == 50)  # only need one quantile
  if (nrow(opp_stats) == 0) {
    # No data found
    return(tibble::tibble())
  }

  # Only keep file_id and date as datetime object
  date_df <- dplyr::select(
    dplyr::mutate(opp_stats, file_id=file, date=lubridate::ymd_hms(date)),
    file_id,
    date
  )
  opp_files <- list.files(opp.dir, pattern=paste0("*.opp.parquet"), full.names=TRUE)
  # TODO this will throw an error if something in date_df isn't in opp_files
  # do something to handle it
  window_df <- add_window_paths(date_df, opp_files)  # associate files with parquet files

  dfs <- list()
  # TODO check for files with no window file matches
  for (opp_path in unique(window_df$window_path)) {
    opp_df <- dplyr::filter(
      tibble::as_tibble(arrow::read_parquet(opp_path, col_select={{ col_select }})),
      file_id %in% window_df$file_id
    )
    dfs[[length(dfs)+1]] <- opp_df
  }
  # Concatenate and remove unused levels in factors
  df <- dplyr::mutate_if(dplyr::bind_rows(dfs), is.factor, forcats::fct_drop)
  return(df)
}

#' Get a tibble of per particle population classifications by file and quantile
#'
#' @param db SQLite3 database file path.
#' @param vct.dir VCT file directory.
#' @param file.name File name with julian day directory.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   tidy-selection. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of per particle population classifications. If no data is
#'   found a tibble with zero rows will be returned.
#' @examples
#' \dontrun{
#' vct <- get.vct.by.file(vct.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.vct.by.file <- function(db, vct.dir, file.name, col_select=NULL) {
  file.name <- clean.file.path(file.name)
  col_select <- rlang::enquo(col_select)
  if (rlang::quo_is_null(col_select)) {
    col_select <- NULL
  } else {
    # Alway enforce dates and file_ids as the first columns
    col_select <- rlang::expr(c(date, file_id, {{ col_select }}))
  }
  opp_stats <- tibble::as_tibble(get.opp.stats.by.file(db, file.name))
  opp_stats <- dplyr::filter(opp_stats, quantile == 50)  # only need files and dates for one quantile
  if (nrow(opp_stats) == 0) {
    # No data found
    return(tibble::tibble())
  }

  # Only keep file_id and date as datetime object
  date_df <- dplyr::select(
    dplyr::mutate(opp_stats, file_id=file, date=lubridate::ymd_hms(date)),
    file_id,
    date
  )
  vct_files <- list.files(vct.dir, pattern=paste0("*.vct.parquet"), full.names=TRUE)
  # TODO this will throw an error if something in date_df isn't in vct_files
  # do something to handle it
  window_df <- add_window_paths(date_df, vct_files)  # associate files with parquet files
  # TODO check for no match
  vct_path <- window_df$window_path[1]
  vct_df <- dplyr::filter(
    tibble::as_tibble(arrow::read_parquet(vct_path, col_select={{ col_select }})),
    file_id == file.name
  )
  vct_df <- dplyr::mutate_if(vct_df, is.factor, forcats::fct_drop)
  return(vct_df)
}

#' Get data frame of per particle population classifications selecting by date.
#'
#' Datetime string formats should be in the form "2020-07-31 10:00".
#'
#' @param db SQLite3 database file path.
#' @param vct.dir VCT file directory.
#' @param start.date Start date string, inclusive.
#' @param end.date End date string, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   tidy-selection. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Data frame of per particle population classifications. If no data is
#'   found a data frame with zero rows will be returned.
#' @examples
#' \dontrun{
#' vct <- get.vct.by.date(db, vct.dir, "2020-07-31 10:00", "2020-07-31 12:00"
#'                        col_select=c(ends_with("q50"), starts_with("diam_mid"))
#' }
#' @export
get.vct.by.date <- function(db, vct.dir, start.date, end.date, outliers=TRUE,
                            col_select=NULL) {
  col_select <- rlang::enquo(col_select)
  if (rlang::quo_is_null(col_select)) {
    col_select <- NULL
  } else {
    # Alway enforce dates and file_ids as the first columns
    col_select <- rlang::expr(c(date, file_id, {{ col_select }}))
  }

  opp_stats <- tibble::as_tibble(get.opp.stats.by.date(
    db, start.date=start.date, end.date=end.date, outliers=outliers
  ))
  opp_stats <- dplyr::filter(opp_stats, quantile == 50)  # only need one quantile
  if (nrow(opp_stats) == 0) {
    # No data found
    return(tibble::tibble())
  }

  # Only keep file_id and date as datetime object
  date_df <- dplyr::select(
    dplyr::mutate(opp_stats, file_id=file, date=lubridate::ymd_hms(date)),
    file_id,
    date
  )
  vct_files <- list.files(vct.dir, pattern=paste0("*.vct.parquet"), full.names=TRUE)
  # TODO this will throw an error if something in date_df isn't in vct_files
  # do something to handle it
  window_df <- add_window_paths(date_df, vct_files)  # associate files with parquet files

  dfs <- list()
  # TODO check for files with no window file matches
  for (vct_path in unique(window_df$window_path)) {
    vct_df <- dplyr::filter(
      tibble::as_tibble(arrow::read_parquet(vct_path, col_select={{ col_select }})),
      file_id %in% window_df$file_id
    )
    dfs[[length(dfs)+1]] <- vct_df
  }
  # Concatenate and remove unused levels in factors
  df <- dplyr::mutate_if(dplyr::bind_rows(dfs), is.factor, forcats::fct_drop)
  return(df)
}

#' Create an empty EVT data frame
#'
#' @return EVT data frame with no rows
#' @examples
#' \dontrun{
#' evt <- empty_evt()
#' }
empty_evt <- function() {
  df <- data.frame(matrix(ncol = length(EVT.HEADER), nrow = 0))
  colnames(df) <- EVT.HEADER
  return(df)
}

#' Create an empty OPP data frame
#'
#' @return OPP data frame with no rows
#' @examples
#' \dontrun{
#' evt <- empty_opp()
#' }
empty_opp <- function() {
  return(empty_evt())
}

#' Add intervals and window paths for each date in df.
#'
#' @param df Dataframe with a "date" column of POSIXct dates.
#' @param window_paths Time-windowed SeaFlow file paths.
#' @return A copy of df with a new lubridate::interval column "interval" and a
#'   a new character column "window_path" of a time-windowed file path matching
#'   each date. New values are NA if a date doesn't match a time-windowed
#'   seaflow file path.
add_window_paths <- function(df, window_paths) {
  if (! "date" %in% colnames(df)) {
    stop("date column missing from df in add_window_paths()")
  }
  window_df <- tibble::tibble(window_path=window_paths)
  window_df <- add_window_paths_intervals(window_df)
  # Set all to NA, meaning not found by default
  df$interval <- lubridate::interval(start=NA, end=NA)
  df$window_path <- NA
  for (i in 1:nrow(df)) {
    matched_intervals <- lubridate::`%within%`(df$date[i], window_df$interval)
    # If more than one interval found for a date throw an error, this should
    # never happen.
    if (sum(matched_intervals) > 1) {
      stop(paste("Found more than one window file for", df[i, ]))
    }
    if (sum(matched_intervals) == 1) {
      df$interval[i] <- window_df$interval[matched_intervals]
      df$window_path[i] <- window_df$window_path[matched_intervals]
    }
    # else we're left with the default NA for no matches
  }
  return(df)
}

#' Get datetime interval for time-windowed SeaFlow file paths.
#'
#' @param window_df Dataframe with column "window_path" for SeaFlow
#'   time-windowed file paths.
#' @return A copy of window_df with a new "interval" column of
#'   lubridate::interval, inclusive at the start boundaries and exclusive at
#'   the end boundary.
add_window_paths_intervals <- function(window_df) {
  # Given a list of seaflow time-windowed file paths, return a list of
  # lubridate::interval objects.
  filenames <- basename(window_df$window_path)
  pattern <- paste0("^[^.]+\\.[123456789]\\d*H\\.(vct|opp)\\.")
  good_files <- grepl(pattern, filenames)
  if (!all(good_files)) {
    stop(paste0(
      "some files dont' look like SeaFlow OPP/VCT windowed files\n",
      paste(window_df$window_path[!good_files], collapse="\n")
    ))
  }

  parts <- strsplit(filenames, ".", fixed=TRUE)
  timestamps <- sapply(parts, function(x) x[1])
  windows <- sapply(parts, function(x) x[2])
  hours <- window_hours(windows)

  # subtract 1 millisecond from the end to get exclusive interval end points
  # at +1H
  t0s <- parse_file_dates(timestamps)
  t1s <- t0s + lubridate::hours(hours) - lubridate::milliseconds(1)
  intervals <- lubridate::interval(t0s, t1s)
  if (length(intervals) != length(window_df$window_path)) {
    stop("length(intervals) != length(window_df$window_path) in add_window_paths_intervals()")
  }
  window_df$interval <- intervals
  return(window_df)
}

window_hours <- function(windows) {
  # Return numeric hours from time window size strings
  # e.g. 1H for 1 hour, 10H for 10 hours
  pattern <- "^[123456789]\\d*H$"
  good <- grepl(pattern, windows)
  if (!all(good)) {
    stop(paste("some time windows are poorly formatted: ", windows[!good]))
  }
  hours <- as.integer(substr(windows, 1, nchar(windows)-1))
  return(hours)
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


#' Manipulate the size distribution created by create_PSD(). 
#' Calculate the sum of particles in each size class over specific temporal resolution; transform the header
#'
#' @param PSD Particle size disitribution created by create_PSD().
#'  i.e., a tibble of size distribution over time. First column must be time (POSIXt class object);
#'  Second column must name of the population; other columns represent the different size classes. 
#'  Size classes can represent either diameter or carbon quota (assuming spherical particles).
#' @param time.step Time step over which to sum the number of particles in each size class. Default 1 hour, must be higher than 3 minutes
#' @param Qc.to.diam Convert carbon quotas to diameter as described in
#'  Menden-Deuer, S. and Lessard, E. J. Carbon to volume relationships for dinoflagellates, diatoms, and other protist plankton.
#'  Limnol. Oceanogr. 45, 569–579 (2000).
#' @param abundance.to.biomass Calculate carbon biomass in each population (i.e. cell abundance x Qc)
#' @param interval.to.geomean Transform size class intervals to geometric mean values 
#' (i.e. convert breaks (min, max] to geometric mean defined as sqrt(mean*max). 
#' @return Size distribution 
#' @name transform_PSD
#' @examples
#' \dontrun{
#' PSD <- transform_PSD(PSD, time.step="1 hour")
#' }
#' @export
transform_PSD <- function(PSD, time.step="1 hour", 
                                        Qc.to.diam=FALSE, 
                                        interval.to.geomean=FALSE,
                                        abundance.to.biomass=FALSE){
  
  # Check that 'time' is a POSIXt class object 
  if(! lubridate::is.POSIXt(PSD$date)){
  print("Date is not recognized as POSIXt class")
  stop
  }

  # Check that 'pop' column is there 
  if(!any(names(PSD)=='pop')){
    print("column 'pop' is missing")
  stop
  }

   # Calculate the mean in each size class over new time interval
  if(!is.null(time.step)){
    PSD  <- PSD  %>%
              dplyr::group_by(date = cut(date, breaks=time.step), pop) %>%
              dplyr::summarise_all(mean)
  }                    



  # Menden-Deuer, S. & Lessard conversion factors
  d <- 0.261; e <- 0.860
  # select column that have PSD data
  clmn <- grep("]", names(PSD))
  # convert size interval (factors) into data.frame
  breaks <- strsplit(sub("\\]","",sub("\\(","",colnames(PSD)[clmn])),",")


  if(Qc.to.diam){
    #convert Qc into diam using the Menden-Deuer conversion
    b <- lapply(breaks, function(x) round(2*(3/(4*pi)*(as.numeric(x)/d)^(1/e))^(1/3),6))
    colnames(PSD)[clmn] <- sub("\\)","\\]", sub("c","",as.character(b)))
  }

  if(interval.to.geomean){
    # transform size class intervals to mean values (i.e. convert breaks (min, max] to geom mean). 
    if(Qc.to.diam){
      midval <- unlist(list(lapply(b, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
    }else{
      midval <- unlist(list(lapply(breaks, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
      }
    colnames(PSD)[clmn] <- round(midval,4)
  }
  
  if(abundance.to.biomass){
    # calculate biomass in each bin (ugC L-1) = Qc(pgC cell-1) x Abundance (cells uL-1)
    midval <- unlist(list(lapply(breaks, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
    PSD[,clmn] <-  t(diag(midval) %*%  t(as.matrix(PSD[,clmn])))
  }

  # time converted to factor needs to be converted back to POSIXt
  PSD$date <- as.POSIXct(PSD$date, tz='GMT')

  return(PSD)

}

#' Clean the stat table by selecting the closest refractive index to the reference diameter for a particular population,
#' keep only the clean data (flag ==0 ) and remove percentile column
#'
#' @param db SQLite3 database file path.
#' @param pop Name of the population. Can be either "prochloro", "synecho", "picoeuk", "croco"
#' @param ref_diam Diameter value (in micron)
#' @return A cleaned stat table with corrected refractive index
#' @export


get.clean.stat.table <- function(db, pop="prochloro", ref_diam=0.54){

  cruise <- sub(".db","",basename(db))
  print(cruise)
  stat <- tibble::as_tibble(get.stat.table(db))
  stat <- stat.calibration(stat, cruise)

  ### Select the appropriate refractive index for each population
  ri <- tibble::tibble(lwr=1.055, mid=1.032, upr=1.017)

  # Select the appropriate refractive index
  ref <- as.numeric(ref_diam)
  phyto <- as.character(pop)

  p  <- stat %>%
        dplyr::filter(flag == 0, quantile == 2.5, pop == phyto) %>%
        dplyr::select(diam_lwr_med, diam_mid_med, diam_upr_med) %>%
        dplyr::summarise_all(mean)
  best <- which(abs(p - ref) == min(abs(p - ref)))
  refrac <- c("lwr","mid","upr")[best[1]]
        
  if(is.na(refrac)) refrac <- "mid" # case when there is no population of interest

  print(paste0("best refractive index for ",phyto," : ",ri[refrac] ," (",refrac, ")"))
            
  # choice of refractive index for Prochloro, Synecho, picoeuk and Croco, respectively
  n <- c(refrac, refrac, "lwr","lwr") 

  # select the appropriate data
  clean <- tibble::tibble()
  i <- 1
  for(phyto in c("prochloro","synecho","picoeuk","croco")){
    p <- stat %>%
          dplyr::filter(flag == 0, quantile == 2.5, pop == phyto) %>%
          dplyr::select(time,lat,lon, pop, abundance, contains(c(paste0(n[i],"_med"),paste0(n[i],"_mean")))) %>%
          dplyr::rename(diam = contains(paste0("diam_",n[i],"_med")), Qc = contains(paste0("Qc_",n[i],"_med"))) %>%
          dplyr::mutate(biomass = abundance * Qc) %>%
          dplyr::select(!ends_with("mean"))
    clean <- clean %>% dplyr::bind_rows(p)
    i <- i + 1
  }

  return(clean)

}


#' Clean the Particle size disitrubution by selecting the closest refractive index to the reference diameter for a particular population,
#' keep only the clean data (flag ==0 ) 
#'
#' @param PSD Particle size disitribution created by create_PSD().
#' @param pop Name of the population. Can be either "prochloro", "synecho", "picoeuk", "croco"
#' @param ref_diam Diameter value (in micron)
#' @return A cleaned Particle Size distribution with corrected refractive index
#' @export
#' 

get.clean.PSD <- function(PSD, pop="prochloro", ref_diam=0.54){

  ref <- as.numeric(ref_diam)
  phyto <- as.character(pop)

  ### Select the appropriate refractive index for each population
  ri <- tibble::tibble(lwr=1.055, mid=1.032, upr=1.017)

  # Select the appropriate refractive index 
  pre.dist <- transform_PSD(PSD, time.step=NULL, Qc.to.diam=TRUE, interval.to.geomean=TRUE, abundance.to.biomass=FALSE)
  pre.dist <- subset(pre.dist, flag==0)
  
  clmn <- grep("]", names(PSD)) # select column that contains the number of cells per diameters
  p_lwr <- colSums(pre.dist[pre.dist$pop==phyto & pre.dist$n == "lwr",clmn], na.rm=T)
    id_lwr <- which(p_lwr == max(p_lwr))
  p_mid <- colSums(pre.dist[pre.dist$pop==phyto & pre.dist$n == "mid",clmn], na.rm=T)
    id_mid <- which(p_mid == max(p_mid))
  p_upr <- colSums(pre.dist[pre.dist$pop==phyto & pre.dist$n == "upr",clmn], na.rm=T)
    id_upr <- which(p_upr == max(p_upr))
  
  diam <- as.numeric(colnames(pre.dist[,clmn]))
  diff <- abs(ref - diam[c(id_lwr, id_mid, id_upr)])
  best <- which(diff == min(diff))
  refrac <- c("lwr","mid","upr")[best[1]]

  if(is.na(refrac)) refrac <- "mid" # case when there is no population of interest

  print(paste0("best refractive index for ",phyto," : ",ri[refrac] ," (",refrac, ")"))
 
  ## Apply best refractive index for each population
  # High refractive index for large cells (Qc_lwr, diam_lwr)
  distribution <- PSD %>%
          dplyr::filter(pop == "prochloro" & n == refrac | 
                pop == "synecho" & n == refrac | 
                pop == "picoeuk" & n == "lwr" | 
                pop == "croco" & n == "lwr") %>%
          dplyr::filter(flag ==0) %>%
          dplyr::select(!c(n, opp_evt_ratio, flag))

  return(distribution)
}