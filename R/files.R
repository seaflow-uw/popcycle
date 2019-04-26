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

#' Check if file path ends with suffix
#'
#' @param path File path to test.
#' @param ending String suffix to test
#' @return TRUE or FALSE if path ends with ending. If either path or ending has
#'   length zero or if ending is longer than path, return FALSE.
#' @examples
#' \dontrun{
#' endswith("foo/bar.txt", ".txt") # TRUE
#' endswith("foo/bar.txt", ".gz")  # FALSE
#' }
#' @export
endswith <- function(path, ending) {
  psize <- nchar(path)
  esize <- nchar(ending)
  if (psize > 0 && esize > 0 && psize >= esize) {
    return(substr(path, psize - esize + 1, psize) == ending)
  }

  return(FALSE)
}



#' Extract dawn and dusk times from a time series of PAR values
#'
#' Extracts the time that dawn and dusk occur from a time series consisting of PAR values (light intensity).
#' Dawn is defined as the point where the par value goes from night -> day.
#' Dusk is defined as the point where the par value goes from day -> night.
#' This is useful for gating purposes for flow- cytometry.
#' Dawn and dusk tend to represent the extrema of flourences for phytoplankton.
#' @param x A data frame with two columns. The first column must be time values
#'    in as.POSIXct format. The second column must be PAR values.
#' @param cutoff An integer representing the smallest par value that is considered
#'  daytime.
#' @return A vector consisting of dawn and dusk time values in as.POSIXct format
#' @examples
#' \dontrun{
#' par.data.csv <- system.file("extdata/par_data.csv", package="popcycle")
#' par.data <- read.csv(par.data.csv)
#' par.data$date <- as.POSIXct(par.data$date, format = "%FT%T", tz = "GMT")
#' dawn.dusk.times <- get.dawn.dusk.time(par.data, 10)
#' }
#' @export
get.dawn.dusk.time <- function(x, cutoff) {
    # assign names to the time and par parameters
    time <- x[,1]
    par <- x[,2]

    # We will define dawn and dusk times to occur when par goes from above 10 -> below 10,
    # or from below 10 -> above 10
    above <- par > cutoff
    intersect <- which(diff(above)!=0)

    # We will then make sure we are only getting points that fall within the sequential pattern
    # of natural dusk and dawns, AKA they must be separated by same time intervals

    #revision <- intersect[which(diff(intersect) > mean(diff(intersect)))]
    #dawn.dusk <- time[revision]

    dawn.dusk <- time[intersect]

    return(dawn.dusk)
}

#' file_transfer
#'
#' @return None
#' @export
file_transfer <- function(evt.dir, instrument.dir){

  last.evt <- get.latest.evt(evt.dir)
  file.list <- list.files(instrument.dir, recursive=T)
  sfl.list <- file.list[grepl('.sfl', file.list)]
  file.list <- file.list[-length(file.list)] # remove the last file (opened file)
  file.list <- sort(file.list[!grepl('.sfl', file.list)])

  id <- match(last.evt, file.list)

  if(is.na(id)){
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


#' select_files_in
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
