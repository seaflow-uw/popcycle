rerun_filter <- function(start_day, start_timestamp, end_day, end_timestamp) {
  files <- files_in_range(start_day, start_timestamp, end_day, end_timestamp)
  params <- read.csv(filter.param.location)
  if (is.null(params$notch) || is.null(params$width)) {
    stop('Notch or Width is not defined; skipping filtering.')
  }
  
  for (i in 1:length(files)) {
    evt_file = files[i]
    evt <- readSeaflow(paste0(evt.location, evt_file))
    
    opp <- filter_evt(evt, filter.notch, width = params$width, notch = params$notch)
    # delete old opp entries if they exist so we keep cruise/file/particle distinct
    .delete_opp_by_file(evt_file)
    # store opp
    upload_opp(opp_to_db_opp(opp, cruise.id, evt_file))
    
    if (length(list.files(path=gating.param.location, pattern= ".csv", full.names=TRUE)) > 0) {
      vct <- classify_opp(opp, gating, gating.param.location)
      # delete old vct entries if they exist so we keep cruise/file/particle distinct
      .delete_vct_by_file(evt_file)
      # store vct
      upload_vct(vct_to_db_vct(vct, cruise.id, evt_file, 'Manual Gating'))
    }
  }
}

rerun_gating <- function(start_day, start_timestamp, end_day, end_timestamp) {
  files <- files_in_range(start_day, start_timestamp, end_day, end_timestamp)
  params <- read.csv(filter.param.location)
  if (length(list.files(path=gating.param.location, pattern= ".csv", full.names=TRUE)) == 0) {
    stop('No gates are defined; stopping gating.')
  }
  
  for (i in 1:length(files)) {
    evt_file = files[i]    
    opp <- get_opp_by_file(evt_file)
    vct <- classify_opp(opp, gating, gating.param.location)
    # delete old vct entries if they exist so we keep cruise/file/particle distinct
    .delete_vct_by_file(evt_file)
    # store vct
    upload_vct(vct_to_db_vct(vct, cruise.id, evt_file, 'Manual Gating'))
  }
}