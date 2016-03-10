get.evt.files <- function(evt.dir) {
  file.list <- list.files(evt.dir, recursive=T)
  if (length(file.list) == 0) {
    print(paste("no evt files found in", evt.dir))
    return (file.list)
  }
  # regexp to match both types of EVT files
  #   - 37.evt (old style)
  #   - 2014-05-15T17-07-08+0000 or 2014-07-04T00-03-02+00-00 (new style)
  # In the new style the final timezone offset may not always be UTC (00-00)
  # so be sure to correctly parse it in all code.
  regexp <- "/?[0-9]+\\.evt(\\.gz)?$|/?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}[+-][0-9]{2}-?[0-9]{2}(\\.gz)?$"
  id <- grep(regexp,file.list)
  file.list <- file.list[id]
  print(paste(length(file.list), "evt files found"))
  return (sort(file.list))
}

get.latest.evt.with.day <- function(evt.dir) {
  file.list <- get.evt.files(evt.dir)
  n <- length(file.list)
  return (file.list[n])
}

get.latest.evt <- function(evt.dir) {
  return (basename(get.latest.evt.with.day(evt.dir)))
}

files.in.range <- function(start.day, start.timestamp, end.day, end.timestamp, evt.dir) {
  file.list <- get.evt.files(evt.dir)
  start.file = paste(start.day, start.timestamp, sep='/')
  end.file = paste(end.day, end.timestamp, sep='/')

  if(!any(file.list == start.file)) {
    stop(paste("Could not find file", start.file))
  }

  if(!any(file.list == end.file)) {
    stop(paste("Could not find file", end.file))
  }

  start.index = which(file.list == start.file)
  end.index = which(file.list == end.file)

  return(file.list[start.index:end.index])
}


file.transfer <- function(evt.dir, instrument.dir){

  last.evt <- get.latest.evt.with.day(evt.dir)
  file.list <- list.files(instrument.dir, recursive=T)
  sfl.list <- file.list[grepl('.sfl', file.list)]
  file.list <- file.list[-length(file.list)] # remove the last file (opened file)
  file.list <- sort(file.list[!grepl('.sfl', file.list)])

  id <- match(last.evt, file.list)

  if(length(id) == 0){
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

is.new.style.file <- function(file.name) {
  # regexp to new style EVT file names
  #   - 2014-05-15T17-07-08+0000 or 2014-07-04T00-03-02+00-00 (new style)
  regexp.new <- "/?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}\\+[0-9]{2}-?[0-9]{2}(\\.gz)?$"
  return(length(grep(regexp.new, file.name)) == 1)
}

# Convert an EVT/OPP/VCT file path to a form suitable for storage in the SQLite
# db. Returned file path should be julian_day/EVT_file_name
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
