
rerun.filter <- function(start.day, start.timestamp, end.day, end.timestamp) {
  files <- files.in.range(start.day, start.timestamp, end.day, end.timestamp)

  params <- read.csv(paste(param.filter.location, 'filter.csv', sep='/'))
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  for (i in 1:length(files)) {
  	#if we get an error, move to next file
    tryCatch({
    	evt.file = files[i]
	    file.name = basename(evt.file)
   		print(paste('Filtering', evt.file))
	    evt <- readSeaflow(paste(evt.location, evt.file, sep='/'))
	    opp <- filter.evt(evt, filter.notch, width = params$width, notch = params$notch)
	    # delete old opp entries if they exist so we keep cruise/file/particle distinct
	    .delete.opp.by.file(file.name)
	    # store opp
	    print('Uploading filtered particles to database')
	    upload.opp(opp.to.db.opp(opp, cruise.id, file.name))
    
	    if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) > 0) {
	      print(paste('Classifying', evt.file))
	      vct <- classify.opp(opp, Gating, param.gate.location)
	      # delete old vct entries if they exist so we keep cruise/file/particle distinct
	      .delete.vct.by.file(file.name)
	      # store vct
	      print('Uploading labels to the database')
	      upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'))
	      # TODO: insert statistics 
	    }
	}, error = print(paste("Encountered error with file", evt.file))
	)
  }
}

rerun.gating <- function(start.day, start.timestamp, end.day, end.timestamp) {
  files <- files.in.range(start.day, start.timestamp, end.day, end.timestamp)
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    stop('No gate paramters yet; no gating.')
  }
  
  for (i in 1:length(files)) {
  	tryCatch({
    	evt.file = files[i]
    	file.name = basename(evt.file)
    	opp <- get.opp.by.file(file.name)
    	print(paste('Classifying', evt.file))
    	vct <- classify.opp(opp, Gating, param.gate.location)
    	# delete old vct entries if they exist so we keep cruise/file/particle distinct
    	.delete.vct.by.file(file.name)
    	# store vct
    	print('Uploading labels to the database')
    	upload.vct(vct.to.db.vct(vct, cruise.id, file.name, 'Manual Gating'))
    	# TODO: insert statistics
    }, error = print(paste("Encountered error with file", evt.file))
    )
  }
}
