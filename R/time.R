# Date and time manipulations

#' Convert timestamp component of SeaFlow filenames to POSIXct.
#'
#' @param file_timestamps Character vector of timestamp components of SeaFlow
#'  filenames matching the format YYYY-MM-DDTHH-MM-SS+HH-MM, for example
#'  "2020-07-23T14-04-05+00-00". This corresponds to the RFC3339 formatted
#'  timestamp "2020-07-23T14:04:05+00:00".
#' @return POSIXct vector
#' @examples
#' \dontrun{
#' dates <- parse_file_dates(
#'   c("2020-07-23T14:04:05+00:00", "2020-07-25T14:04:05+00:00")
#' )
#' }
parse_file_dates <- function(file_timestamps) {
  parts <- strsplit(file_timestamps, "T", fixed=TRUE)
  date_parts <- sapply(parts, function(x) x[1])
  time_parts <- stringr::str_replace_all(sapply(parts, function(x) x[2]), "-", ":")
  final_stamps <- paste(date_parts, time_parts)
  return(lubridate::ymd_hms(final_stamps))
}

#' Convert a date string to format suitable for popcycle db date comparison.
#'
#' @param date.string Date text in format YYYY-MM-DD HH:MM.
#' @return Date text as YYYY-MM-DDTHH:MM:00.
#' @examples
#' \dontrun{
#' db.date <- date.to.db.date("2014-01-24 13:03")
#' }
date.to.db.date <- function(date.string) {
  return(POSIXct.to.db.date(string.to.POSIXct(date.string)))
}

#' Returns a POSIXct object for a human readable date string.
#'
#' @param date.string Date in format YYYY-MM-DD HH:MM.
#' @return POSIXct object.
#' @examples
#' \dontrun{
#' date.ct <- string.to.POSIXct("2014-01-24 13:03")
#' }
string.to.POSIXct <- function(date.string) {
  # Make POSIXct objects in GMT time zone
  date.ct <- as.POSIXct(strptime(date.string, format="%Y-%m-%d %H:%M", tz="GMT"))
  if (is.na(date.ct)) {
    stop(paste("wrong format for date.string parameter : ", date.string,
               "instead of ", "%Y-%m-%d %H:%M"))
  }
  return(date.ct)
}

#' Convert a POSIXct date objectformat suitable for popcycle db date comparison.
#'
#' @param date.ct POSIXct date object.
#' @return Date text as YYYY-MM-DDTHH:MM:00.
#' @examples
#' \dontrun{
#' db.date <- POSIXct.to.db.date(date.ct)
#' }
POSIXct.to.db.date <- function(date.ct) {
  return(format(date.ct, "%Y-%m-%dT%H:%M:00"))
}

#' Get current UTC datetime as RFC3339 string suitable for entry into db
#'
#' @return Date text as YYYY-MM-DDTHH:MM:SS+00:00."
#' @examples
#' \dontrun{
#' datetime.str <- RFC3339.now()
#' }
RFC3339.now <- function() {
  # Now in local time POSIXct
  now.local <- Sys.time()
  # Change timezone to UTC
  attr(now.local, "tzone") <- "UTC"
  return(format(now.local, format="%FT%H:%M:%S+00:00"))
}