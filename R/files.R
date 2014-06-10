get_evt_list <- function() {
  file_list <- list.files(evt.location, recursive=T)
  if (length(file_list) == 0) {
    return (file_list)
  }
  #drop .sfl files
  file_list <- file_list[!grepl('.sfl', file_list)]
  return (sort(file_list))
}

get_latest_file <- function() {
  file_list <- get_evt_list()
  n <- length(file_list)
  return (file_list[n])
}

files_in_range <- function(start_day, start_timestamp, end_day, end_timestamp) {
  file_list <- get_evt_list()
  start_file = paste(start_day, start_timestamp, sep='/')
  end_file = paste(end_day, end_timestamp, sep='/')
  
  if(!any(file_list == start_file)) {
    stop(paste("Could not find file", start_file))
  }
  
  if(!any(file_list == end_file)) {
    stop(paste("Could not find file", end_file))
  }
  
  start_index = which(file_list == start_file)
  end_index = which(file_list == end_file)
  
  return(file_list[start_index:end_index])
}


file.transfer <- function(){

  last.evt <- get_latest_file()
  file_list <- list.files(instrument.location, recursive=T)
  file_list <- file_list[!grepl('.sfl', file_list)]

  id <- match(last.evt, file_list)

  if(length(id) == 0){
    day <- unique(dirname(file_list))
      for(d in day) system(paste0("mkdir ",evt.location,"/",d))
    print(paste0("scp ",instrument.location,"/",file_list," ", evt.location,"/",file_list))
    system(paste0("scp ",instrument.location,"/",file_list," ", evt.location,"/",file_list))
  }
  else{
    file_list <- file_list[id:length(file_list)]
    day <- unique(dirname(file_list))
      for(d in day) system(paste0("mkdir ",evt.location,d))
    print(paste0("scp ",instrument.location,"/",file_list," ", evt.location,"/",file_list))
    system(paste0("scp ",instrument.location,"/",file_list," ", evt.location,"/",file_list))

  }
}