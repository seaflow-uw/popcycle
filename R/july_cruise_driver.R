# set width and notch, log old parameters if they exist
setFilterParams <- function(...) {
  # TODO(hyrkas): implement
}

# set gates, log old parameters if they exist
setManualGates <- function(...) {
  # TODO(hyrkas): implement
}

evaluate_evt <- function(evt_file) {
  #filter evt
  evt <- readSeaflow(evt_file)
  
  #if we don't have filter parameters yet
  if (!file.exists(filter.param.location)) {
    return()
  }
  
  params <- read.csv(filter.param.location)
  
  if (is.null(params$notch) || is.null(params$width)) {
    print('Notch or Width is not defined; skipping filtering.')
    return()
  }
  
  opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
  
  #store opp
  upload_opp(opp_to_db_opp(opp, cruise.id, evt_file))
  
  #classify opp
  
  #if we don't have gating parameters yet
  if (!file.exists(gating.param.location)) {
    return()
  }
  
  # TODO(hyrkas): implement manual_gate function
  vct <- classify_opp(opp, manual_gate, gating.param.location)
  
  #store vct
  upload_vct(vct_to_db_vct(vct, cruise.id, evt_file, 'Manual Gating'))
}