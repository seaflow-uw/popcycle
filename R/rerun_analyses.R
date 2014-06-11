rerun_filter <- function(start_day, start_timestamp, end_day, end_timestamp) {
  files <- files_in_range(start_day, start_timestamp, end_day, end_timestamp)
  params <- read.csv(paste(param.filter.location, 'filter.csv', sep='/'))
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  for (i in 1:length(files)) {
  	#if we get an error, move to next file
    tryCatch({
    	evt_file = files[i]
	    file_name = basename(evt_file)
   		print(paste('Filtering', evt_file))
	    evt <- readSeaflow(paste(evt.location, evt_file, sep='/'))
    
	    opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
	    # delete old opp entries if they exist so we keep cruise/file/particle distinct
	    .delete_opp_by_file(file_name)
	    # store opp
	    print('Uploading filtered particles to database')
	    upload_opp(opp_to_db_opp(opp, cruise.id, file_name))
    
	    if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) > 0) {
	      print(paste('Classifying', evt_file))
	      vct <- classify_opp(opp, Gating, param.gate.location)
	      # delete old vct entries if they exist so we keep cruise/file/particle distinct
	      .delete_vct_by_file(file_name)
	      # store vct
	      print('Uploading labels to the database')
	      upload_vct(vct_to_db_vct(vct, cruise.id, file_name, 'Manual Gating'))
	      # TODO: insert statistics 
	    }
	}, error = print(paste("Encountered error with file", evt_file))
	)
  }
}

rerun_gating <- function(start_day, start_timestamp, end_day, end_timestamp) {
  files <- files_in_range(start_day, start_timestamp, end_day, end_timestamp)
  if (length(list.files(path=param.gate.location, pattern= ".csv", full.names=TRUE)) == 0) {
    stop('No gate paramters yet; no gating.')
  }
  
  for (i in 1:length(files)) {
  	tryCatch({
    	evt_file = files[i]
    	file_name = basename(evt_file)
    	opp <- get_opp_by_file(file_name)
    	print(paste('Classifying', evt_file))
    	vct <- classify_opp(opp, Gating, param.gate.location)
    	# delete old vct entries if they exist so we keep cruise/file/particle distinct
    	.delete_vct_by_file(file_name)
    	# store vct
    	print('Uploading labels to the database')
    	upload_vct(vct_to_db_vct(vct, cruise.id, file_name, 'Manual Gating'))
    	# TODO: insert statistics
    }, error = print(paste("Encountered error with file", evt_file))
    )
  }
}
