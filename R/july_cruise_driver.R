#main function
evaluate.last.evt <- function() {
  evt.file <- get.latest.file.with.day()
  
  if (length(evt.file) == 0) {
    print('No data collected yet.')
    return()
  }

  print(paste('Analyzing', evt.file))
  
  #upload evt count
  file.name = basename(evt.file)
  evt <- readSeaflow(paste(evt.location, evt.file, sep='/'))
  
  print('Uploading evt particle count')
  .delete.evt.count.by.file(file.name)
  upload.evt.count(evt, cruise.id, file.name)
  
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

  print(paste('Filtering', evt.file))
    
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

  insert.stats.for.file(file.name)
}
