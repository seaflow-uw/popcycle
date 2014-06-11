#main function
evaluate.last.evt <- function() {
  evt.file <- get.latest.file.with.day()
  
  if (length(evt.file) == 0) {
    print('No data collected yet.')
    return()
  }
  
  print(paste('Analyzing', evt.file))
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
  
  # file.name for db should get rid of directory structure
  file.name = basename(evt.file)

  print(paste('Filtering', evt.file))
  
  evt <- readSeaflow(paste(evt.location, evt.file, sep='/'))
  
  opp <- filter.evt(evt, filter.notch, width = params$width, notch = params$notch)
  
  #store opp
  
  print('Uploading filtered particles to database')

  .delete.opp.by.file(file.name) 
  upload.opp(opp.to.db.opp(opp, cruise.id, file.name))
  
  #classify opp
  
  #if we don't have gating parameters yet
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    print('No gating parameters have been set; skipping gating.')
    return()
  }
  
  print(paste('Classifying', evt.file))
  
  vct <- classify.opp(opp, Gating, param.gate.location)
  
  #store vct
  print('Uploading labels to the database')

  .delete.vct.by.file(file.name)
  upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'))

  # TODO: insert statistics
}
