
run.filter <- function(evt.list) {
  
  params <- read.csv(paste(param.filter.location, 'filter.csv', sep='/'))
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  i <- 0
  for (evt.file in evt.list) {
    
     message(round(100*i/length(evt.list)), "% completed \r", appendLF=FALSE)

  	#if we get an error, move to next file
    tryCatch({
    #	print(paste('Loading', evt.file))
      evt <- readSeaflow(evt.file)
      
   	#	print(paste('Filtering', evt.file))
	    opp <- filter.evt(evt, filter.notch, width = params$width, notch = params$notch)
	    # delete old opp entries if they exist so we keep cruise/file/particle distinct
	    .delete.opp.by.file(evt.file)
	    # store opp
	 #   print('Uploading filtered particles to database')
	    upload.opp(opp.to.db.opp(opp, cruise.id, evt.file), db=db.name)
    
  #    print(paste('Uploading opp/evt ratio for', evt.file))
      .delete.opp.evt.ratio.by.file(evt.file)
      opp.evt.ratio <- nrow(opp) / nrow(evt)
      upload.opp.evt.ratio(opp.evt.ratio, cruise.id, evt.file, db=db.name)
   
   	}, error = function(e) {print(paste("Encountered error with file", evt.file))},
	finally = {print(paste("Finished with file", evt.file))}
	)
    
    i <-  i + 1
    flush.console()

  }
}

run.gating <- function(opp.list) {
  
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    stop('No gate paramters yet; no gating.')
  }
  
  i <- 0
  for (opp.file in opp.list) {
  	
     message(round(100*i/length(opp.list)), "% completed \r", appendLF=FALSE)

    tryCatch({
     # print(paste('Loading', opp.file))
      opp <- get.opp.by.file(opp.file)
  #  	print(paste('Classifying', opp.file))
    	vct <- classify.opp(opp, ManualGating, param.gate.location)
    	# delete old vct entries if they exist so we keep cruise/file/particle distinct
    	.delete.vct.by.file(opp.file)
    	# store vct
   # 	print('Uploading labels to the database')
    	upload.vct(vct.to.db.vct(vct, cruise.id, opp.file, 'Manual Gating'), db=db.name)

   #   print('Updating stat')
      insert.stats.for.file(opp.file, db=db.name)
    }, error = function(e) {print(paste("Encountered error with file", opp.file))},
    finally = {print(paste("Finished with file", opp.file))}
    )
    
    i <-  i + 1
    flush.console()

  }
}
