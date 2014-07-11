get.evt.list <- function(...) {
  file.list <- list.files(evt.location, recursive=T)
  if (length(file.list) == 0) {
    return (file.list)
  }
  id <- grep(paste('^[0-9]{1,9}.*','\\.evt','$',sep=''),file.list)
  if(length(id)>0) file.list <- file.list[id]
 
  #drop .sfl files
  file.list <- file.list[!grepl('.sfl', file.list)]
  return (sort(file.list))
}


get.latest.evt.with.day <- function(...) {
  file.list <- get.evt.list(...)
  n <- length(file.list)
  return (file.list[n])
}

get.latest.evt <- function(...) {
  return (basename(get.latest.evt.with.day(...)))
}

files.in.range <- function(start.day, start.timestamp, end.day, end.timestamp) {
  file.list <- get.evt.list()
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


file.transfer <- function(...){

  last.evt <- get.latest.evt.with.day(...)
  file.list <- list.files(instrument.location, recursive=T)
  file.list <- file.list[-length(file.list)] # remove the last file (opened file)
  sfl.list <- file.list[grepl('.sfl', file.list)]
  file.list <- sort(file.list[!grepl('.sfl', file.list)])

  id <- match(last.evt, file.list)

  if(length(id) == 0){
    day <- unique(dirname(file.list))
      for(d in day) system(paste0("mkdir ",evt.location,"/",d))
    print(paste0("scp ",instrument.location,"/",file.list," ", evt.location,"/",file.list))
    system(paste0("scp ",instrument.location,"/",file.list," ", evt.location,"/",file.list, collapse=";"))
    system(paste0("scp ",instrument.location,"/",sfl.list," ", evt.location,"/",sfl.list, collapse=";"))
  }
  else{
    file.list <- file.list[id:length(file.list)]
    day <- unique(dirname(file.list))
      for(d in day) system(paste0("mkdir ",evt.location,"/",d))
    print(paste0("scp ",instrument.location,"/",file.list," ", evt.location,"/",file.list))
    system(paste0("scp ",instrument.location,"/",file.list," ", evt.location,"/",file.list, collapse=";"))
    system(paste0("scp ",instrument.location,"/",sfl.list," ", evt.location,"/",sfl.list, collapse=";"))
  }
}
