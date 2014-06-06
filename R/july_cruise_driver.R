#main function
evaluate_last_evt <- function() {
  evt_file <- get_latest_file()
  
  if (length(evt_file) == 0) {
    print('No data collected yet.')
    return()
  }
  
  print(paste('Analyzing', evt_file))
  #if we don't have filter parameters yet
  if (!file.exists(paste(param.filter.location, 'filter.csv', sep='/'))) {
    print('No filtering parameters have been set; skipping filtering.')
    return()
  }
  
  params <- read.csv(paste(param.filter.location,"filter.csv", sep='/'))
  
  if (is.null(params$notch) || is.null(params$width)) {
    print('Notch or Width is not defined; skipping filtering.')
    return()
  }
  
  #filter evt

  print(paste('Filtering', evt_file))
  
  evt <- readSeaflow(paste0(evt.location, evt_file))
  
  opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
  
  #store opp
  
  print('Uploading filtered particles to database')
  
  upload_opp(opp_to_db_opp(opp, cruise.id, evt_file))
  
  #classify opp
  
  #if we don't have gating parameters yet
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    print('No gating parameters have been set; skipping gating.')
    return()
  }
  
  print(paste('Classifying', evt_file))
  
  vct <- classify_opp(opp, gating, param.gate.location)
  
  #store vct
  print('Uploading labels to the database')
  
  upload_vct(vct_to_db_vct(vct, cruise.id, evt_file, 'Manual Gating'))
}