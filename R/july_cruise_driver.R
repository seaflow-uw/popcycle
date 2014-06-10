#main function
evaluate_last_evt <- function() {
  evt_file <- get_latest_file_with_day()
  
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
  
  # file_name for db should get rid of directory structure
  file_name = basename(evt_file)

  print(paste('Filtering', evt_file))
  
  evt <- readSeaflow(paste(evt.location, evt_file, sep='/'))
  
  opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
  
  #store opp
  
  print('Uploading filtered particles to database')

  .delete_opp_by_file(file_name) 
  upload_opp(opp_to_db_opp(opp, cruise.id, file_name))
  
  #classify opp
  
  #if we don't have gating parameters yet
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    print('No gating parameters have been set; skipping gating.')
    return()
  }
  
  print(paste('Classifying', evt_file))
  
  vct <- classify_opp(opp, Gating, param.gate.location)
  
  #store vct
  print('Uploading labels to the database')

  .delete_vct_by_file(file_name)
  upload_vct(vct_to_db_vct(vct, cruise.id, file_name, 'Manual Gating'))

  # TODO: insert statistics
}
