#main function
evaluate.evt <- function(evt.file) {
  
  if (length(evt.file) == 0) {
    print('No data collected yet.')
    return()
  }

  print(paste('Analyzing', evt.file))
  
  #upload evt count
  file.name = clean.file.name(evt.file)
  print(paste('Loading', evt.file))
  
  tryCatch({
  
  evt <- readSeaflow(evt.file)
 
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
  
  print('Uploading opp/evt ratio')
  .delete.opp.evt.ratio.by.file(file.name)
  opp.evt.ratio <- nrow(opp)/nrow(evt)
  upload.opp.evt.ratio(opp.evt.ratio, cruise.id, file.name)
  
  #classify opp
  
  #if we don't have gating parameters yet
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    print('No gating parameters have been set; skipping gating.')
    return()
  }
  
  print(paste('Classifying', evt.file))
  
  vct <- classify.opp(opp, ManualGating)
  
  #store vct
  print('Uploading labels to the database')

  .delete.vct.by.file(file.name)
  upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'))

  #cytometric diversity
  print("Calculating cytometric diversity")
  opp$pop <- vct
  df <- opp[!(opp$pop == 'beads'),]
  indices <- cytodiv(df, para=c("fsc_small","chl_small","pe"), Ncat=16)

  print('Uploading cytdiv')
  .delete.cytdiv.by.file(file.name)
  upload.cytdiv(indices,cruise.id, file.name)

  #aggregate statistics
  print('Uploading stats')
  insert.stats.for.file(file.name)
  }, error = function(e) {print(paste("Encountered error with file", evt.file))})
  
}
