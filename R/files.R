get.evt.list <- function(evt.loc=evt.location) {
  file.list <- list.files(evt.loc, recursive=T)
  if (length(file.list) == 0) {
    return (file.list)
  }
  id <- grep('[0-9]{1,9}\\.evt$',file.list)
  file.list <- file.list[id]

  return (sort(file.list))
}

get.latest.evt.with.day <- function(evt.loc=evt.location) {
  file.list <- get.evt.list(evt.loc)
  n <- length(file.list)
  return (file.list[n])
}

get.latest.evt <- function(evt.loc=evt.location) {
  return (basename(get.latest.evt.with.day(evt.loc)))
}

files.in.range <- function(start.day, start.timestamp, end.day, end.timestamp, evt.loc=evt.location) {
  file.list <- get.evt.list(evt.loc)
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


file.transfer <- function(evt.loc=evt.location, instrument.loc=instrument.location){

  last.evt <- get.latest.evt.with.day(evt.loc)
  file.list <- list.files(instrument.loc, recursive=T)
  file.list <- file.list[-length(file.list)] # remove the last file (opened file)
  sfl.list <- file.list[grepl('.sfl', file.list)]
  file.list <- sort(file.list[!grepl('.sfl', file.list)])

  id <- match(last.evt, file.list)

  if(length(id) == 0){
    day <- unique(dirname(file.list))
      for(d in day) system(paste0("mkdir ",evt.loc,"/",d))
    print(paste0("scp ",instrument.loc,"/",file.list," ", evt.loc,"/",file.list))
    system(paste0("scp ",instrument.loc,"/",file.list," ", evt.loc,"/",file.list, collapse=";"))
    system(paste0("scp ",instrument.loc,"/",sfl.list," ", evt.loc,"/",sfl.list, collapse=";"))
  }
  else{
    file.list <- file.list[id:length(file.list)]
    day <- unique(dirname(file.list))
      for(d in day) system(paste0("mkdir ",evt.loc,"/",d))
    print(paste0("scp ",instrument.loc,"/",file.list," ", evt.loc,"/",file.list))
    system(paste0("scp ",instrument.loc,"/",file.list," ", evt.loc,"/",file.list, collapse=";"))
    system(paste0("scp ",instrument.loc,"/",sfl.list," ", evt.loc,"/",sfl.list, collapse=";"))
  }
}

# Return a list of EVT files for which there is no OPP data in the database
#
# Args:
#   evt.list = list of EVT file paths, e.g. get.evt.list(evt.location)
#   db = sqlite3 db containing OPP data from EVT files [db.name]
get.empty.evt.files <- function(evt.list, db=db.name) {
  opp.files <- get.opp.files(db)
  return(setdiff(evt.list, opp.files$file))
}

# Return a list of EVT files represented in OPP table
#
# Args:
#   db = sqlite3 db containing OPP data from EVT files [db.name]
get.opp.files <- function(db=db.name) {
  sql <- paste0("SELECT DISTINCT file FROM ", opp.table.name)
  con <- dbConnect(SQLite(), dbname=db)
  oppfiles <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(oppfiles)
}
