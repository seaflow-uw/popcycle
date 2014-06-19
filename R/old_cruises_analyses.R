run.filter.v1 <- function(evt.location){

all.list <- list.files(evt.location, recursive=T, full.name=T)
evt.list <- all.list[!grepl('evt.', all.list)]
files <- evt.list[grepl('evt', evt.list)]

  params <- read.csv(paste(param.filter.location, 'filter.csv', sep='/'))
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  for (i in 1:length(files)) {
  	evt.file = files[i]
    file.name = sub(paste0(evt.location,'/'),'',evt.file)
  	#if we get an error, move to next file
    tryCatch({
    	print(paste('Loading', evt.file))

      evt <- readSeaflow(paste(evt.file, sep='/'))
      
   		print(paste('Filtering', evt.file))
	    opp <- filter.evt(evt, filter.notch, width = params$width, notch = params$notch)
	    # delete old opp entries if they exist so we keep cruise/file/particle distinct
      .delete.opp.by.file(file.name)
	    # store opp
	    print('Uploading filtered particles to database')
	    upload.opp(opp.to.db.opp(opp, cruise.id, file.name),db=db.name)
    
      print(paste('Uploading opp/evt ratio for', file.name))
      .delete.opp.evt.ratio.by.file(file.name)
      opp.evt.ratio <- nrow(opp) / nrow(evt)
      upload.opp.evt.ratio(opp.evt.ratio, cruise.id, file.name, db=db.name)
    

      if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) > 0) {
        print(paste('Classifying', evt.file))
        vct <- classify.opp(opp, ManualGating, param.gate.location)
        # delete old vct entries if they exist so we keep cruise/file/particle distinct
        .delete.vct.by.file(file.name)
        # store vct
        print('Uploading labels to the database')
        upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'), db=db.name)
        print('Updating stat')
        insert.stats.for.file(file.name, db=db.name)
      }

	}, error = function(e) {print(paste("Encountered error with file", file.name))},
	finally = {print(paste("Finished with file", file.name))}
	)
  }

}


run.gating.v1 <- function(opp.location){

all.list <- list.files(opp.location, recursive=T, full.name=T)
opp.list <- all.list[!grepl('opp.', all.list)]
files <- opp.list[grepl('opp', opp.list)]

   
  for (i in 1:length(files)) {
    opp.file = files[i]
    file = sub('.opp','',opp.file)
    file.name = sub(paste0(opp.location,'/'),'',file)
    #if we get an error, move to next file
    tryCatch({
      print(paste('Loading', opp.file))
      opp <- readSeaflow(paste(opp.file, sep='/'))
      # delete old opp entries if they exist so we keep cruise/file/particle distinct
      .delete.opp.by.file(file.name)
      # store opp
      print('Uploading filtered particles to database')
      upload.opp(opp.to.db.opp(opp, cruise.id, file.name), db=db.name)
    
      print(paste('Uploading opp/evt ratio for', file.name))
      .delete.opp.evt.ratio.by.file(file.name)
      n.evt <- readSeaflow(sub('.opp','',opp.file),count.only=TRUE)
      opp.evt.ratio <- nrow(opp)/n.evt
      upload.opp.evt.ratio(opp.evt.ratio, cruise.id, file.name, db=db.name)
   

      if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) > 0) {
        print(paste('Classifying', opp.file))
        vct <- classify.opp(opp, ManualGating, param.gate.location)
        # delete old vct entries if they exist so we keep cruise/file/particle distinct
        .delete.vct.by.file(file.name)
        # store vct
        print('Uploading labels to the database')
        upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'), db=db.name)
        print('Updating stat')
        insert.stats.for.file(file.name, db=db.name)
      }

  }, error = function(e) {print(paste("Encountered error with file", file.name))},
  finally = {print(paste("Finished with file", file.name))}
  )
  }

}



run.stats.v1 <- function(opp.location){

all.list <- list.files(opp.location, recursive=T, full.name=T)
opp.list <- all.list[!grepl('opp.', all.list)]
files <- opp.list[grepl('opp', opp.list)]

   
  for (i in 1:length(files)) {
    opp.file = files[i]
    file = sub('.opp','',opp.file)
    file.name = sub(paste0(opp.location,'/'),'',file)
    #if we get an error, move to next file
    tryCatch({
      print(paste('Loading', opp.file))
      opp <- readSeaflow(paste(opp.file, sep='/'))
      # delete old opp entries if they exist so we keep cruise/file/particle distinct
      .delete.opp.by.file(file.name)
      # store opp
      print('Uploading filtered particles to database')
      upload.opp(opp.to.db.opp(opp, cruise.id, file.name), db=db.name)
    
       print(paste('Uploading opp/evt ratio for', file.name))
      .delete.opp.evt.ratio.by.file(file.name)
      n.evt <- readSeaflow(sub('.opp','',opp.file),count.only=TRUE)
      opp.evt.ratio <- nrow(opp)/n.evt
      upload.opp.evt.ratio(opp.evt.ratio, cruise.id, file.name, db=db.name)
    
      file.number <- sub('.evt','',basename(file.name))
      vct.file <- paste0(opp.file,'.',file.number,'-class.vct')
      vct <- read.table(vct.file, header=T)[,1]
         # delete old vct entries if they exist so we keep cruise/file/particle distinct
        .delete.vct.by.file(file.name)
        # store vct
        print('Uploading labels to the database')
        upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'), db=db.name)
        print('Updating stat')
        insert.stats.for.file(file.name, db=db.name)

  }, error = function(e) {print(paste("Encountered error with file", file.name))},
  finally = {print(paste("Finished with file", file.name))}
  )
  }

}