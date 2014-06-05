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
    print(evt_file)
    evt <- readSeaflow(paste0(evt.location, evt_file))
    
    opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
    # delete old opp entries if they exist so we keep cruise/file/particle distinct
    .delete_opp_by_file(evt_file)
    # store opp
    upload_opp(opp_to_db_opp(opp, cruise.id, evt_file))
    
    if (length(list.files(path=gating.param.location, pattern= ".csv", full.names=TRUE)) > 0) {
      vct <- classify_opp(opp, gating, gating.param.location)
      # delete old vct entries if they exist so we keep cruise/file/particle distinct
      .delete_vct_by_file(evt_file)
      # store vct
      upload_vct(vct_to_db_vct(vct, cruise.id, evt_file, 'Manual Gating'))
    }
  }
}

rerun_gating <- function(start_day, start_timestamp, end_day, end_timestamp) {
  files <- rerun_files(start_day, start_timestamp, end_day, end_timestamp)
  params <- read.csv(paste0(filter.param.location,"filter.csv"))
  if (length(list.files(path=gating.param.location, pattern= ".csv", full.names=TRUE)) == 0) {
    stop('No gate paramters yet; stopping gating.')
  }
  
  for (i in 1:length(files)) {
    evt_file = files[i]    
    opp <- get_opp_by_file(evt_file)
    vct <- classify_opp(opp, gating, gating.param.location)
    # delete old vct entries if they exist so we keep cruise/file/particle distinct
    .delete_vct_by_file(evt_file)
    # store vct
    upload_vct(vct_to_db_vct(vct, cruise.id, evt_file, 'Manual Gating'))
  }
}