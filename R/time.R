# Date and time manipulations

#' Convert timestamp component of SeaFlow filenames to POSIXct.
#'
#' @param timestamps Character vector of timestamp components of SeaFlow
#'  filenames matching the format YYYY-MM-DDTHH-MM-SS+HH-MM, for example
#'  "2020-07-23T14-04-05+00-00". This corresponds to the RFC3339 formatted
#'  timestamp "2020-07-23T14:04:05+00:00".
#' @return POSIXct vector
#' @export
from_filename_date_str <- function(timestamps) {
  parts <- strsplit(timestamps, "T", fixed=TRUE)
  date_parts <- sapply(parts, function(x) x[1])
  time_parts <- stringr::str_replace_all(sapply(parts, function(x) x[2]), "-", ":")
  final_stamps <- paste(date_parts, time_parts)
  return(lubridate::ymd_hms(final_stamps))
}

#' Convert POSIXct date to standard SeaFlow timestamp string.
#'
#' @param dates Vector of POSIXct dates
#' @return Timestamp in the form of 2020-07-25T14:04:05+00:00
#' @export
to_date_str <- function(dates) {
  date_str <- lubridate::format_ISO8601(dates, usetz = TRUE)
  date_str <- stringr::str_c(stringr::str_sub(date_str, 1, -3), ":", stringr::str_sub(date_str, -2, -1))
  return(date_str)
}

#' Convert POSIXct date to standard SeaFlow filename timestamp string.
#'
#' @param date Vector of POSIXct dates
#' @return Timestamp in the form of 2020-07-25T14-04-05+00-00
#' @export
to_filename_date_str <- function(dates) {
  return(stringr::str_replace_all(to_date_str(dates), ":", "-"))
}