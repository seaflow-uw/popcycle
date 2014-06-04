get_evt_list <- function() {
  file_list <- list.files(evt.location, recursive=T)
  #drop .sfl files
  file_list <- file_list[!grepl('.sfl', file_list)]
  return (sort(file_list))
}

get_latest_file <- function() {
  file_list <- get_evt_list()
  n <- length(file_list)
  return (file_list[n])
}

rerun_files <- function(start_day, start_timestamp, end_day, end_timestamp) {
  file_list <- get_evt_list()
  start_file = paste(start_day, start_timestamp, sep="/")
  end_file = paste(end_day, end_timestamp, sep="/")
  
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

rerun_filter <- function(start_day, start_timestamp, end_day, end_timestamp) {
  files <- rerun_files(start_day, start_timestamp, end_day, end_timestamp)
  params <- read.csv(filter.param.location)
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  for (i in 1:length(files)) {
    evt_file = files[i]
    evt <- readSeaflow(paste0(evt.location, evt_file))
    
    opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
    .delete_opp_by_file(evt_file)
    upload_opp(opp_to_db_opp(opp, cruise.id, evt_file))
  }
}