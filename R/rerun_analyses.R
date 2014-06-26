
rerun.filter <- function(start.day, start.timestamp, end.day, end.timestamp) {
  files <- files.in.range(start.day, start.timestamp, end.day, end.timestamp)

  params <- read.csv(paste(param.filter.location, 'filter.csv', sep='/'))
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  for (i in 1:length(files)) {
  	evt.file = files[i]
  	file.name = basename(evt.file)
  	#if we get an error, move to next file
    tryCatch({
    	print(paste('Loading', evt.file))
      evt <- readSeaflow(paste(evt.location, evt.file, sep='/'))
      
   		print(paste('Filtering', evt.file))
	    opp <- filter.evt(evt, filter.notch, width = params$width, notch = params$notch)
	    # delete old opp entries if they exist so we keep cruise/file/particle distinct
	    .delete.opp.by.file(file.name)
	    # store opp
	    print('Uploading filtered particles to database')
	    upload.opp(opp.to.db.opp(opp, cruise.id, file.name), db=db.name)
    
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
        
      #cytometric diversity
        print("Calculating cytometric diversity")
        opp$pop <- vct
        df <- opp[!(opp$pop == 'beads'),]
        indices <- cytodiv(df, para=c("fsc_small","chl_small","pe"), Ncat=16)

        print('Uploading cytdiv')
        upload.cytdiv(indices,cruise.id, file.name)

        print('Updating stat')
        insert.stats.for.file(file.name, db=db.name)
	    }
	}, error = function(e) {print(paste("Encountered error with file", file.name))},
	finally = {print(paste("Finished with file", file.name))}
	)
  }
}

rerun.gating <- function(start.day, start.timestamp, end.day, end.timestamp) {
  files <- files.in.range(start.day, start.timestamp, end.day, end.timestamp)
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    stop('No gate paramters yet; no gating.')
  }
  
  for (i in 1:length(files)) {
  	evt.file = files[i]
  	file.name = basename(evt.file)
  	tryCatch({
      print(paste('Loading', evt.file))
      opp <- get.opp.by.file(file.name)
    	print(paste('Classifying', evt.file))
    	vct <- classify.opp(opp, ManualGating, param.gate.location)
    	# delete old vct entries if they exist so we keep cruise/file/particle distinct
    	.delete.vct.by.file(file.name)
    	# store vct
    	print('Uploading labels to the database')
    	upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'), db=db.name)

      #cytometric diversity
      print("Calculating cytometric diversity")
      opp$pop <- vct
      df <- opp[!(opp$pop == 'beads'),]
      indices <- cytodiv(df, para=c("fsc_small","chl_small","pe"), Ncat=16)

      print('Uploading cytdiv')
      upload.cytdiv(indices,cruise.id, file.name)

      print('Updating stat')
      insert.stats.for.file(file.name, db=db.name)
    }, error = function(e) {print(paste("Encountered error with file", file.name))},
    finally = {print(paste("Finished with file", file.name))}
    )
  }
}
