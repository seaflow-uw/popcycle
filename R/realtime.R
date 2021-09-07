#' Create a metadata tibble for one quantile appopriate for realtime analysis
#'
#' @param db popcycle database file.
#' @param quantile OPP filtering quantile to use.
#' @return A tibble of realtime SFL and OPP table data
#' @export
create_realtime_meta <- function(db, quantile_) {
  quantile_ <- as.numeric(quantile_)
  
  ### Retrieve metadata
  ## Retrieve SFL table
  sfl <- get.sfl.table(db)
  # format time
  sfl$date <- as.POSIXct(sfl$date, format="%FT%T", tz="UTC")
  # retrieve flow rate (mL min-1) of detectable volume
  fr <- flowrate(sfl$stream_pressure, inst=get.inst(db))$flow_rate
  # convert to microL min-1
  fr <- fr * 1000
  # acquisition time (min)
  acq.time <- sfl$file_duration/60
  # volume in microL
  sfl$volume <- round(fr * acq.time , 0)

  ## Retrive OPP table
  # retrieve opp/evt
  opp <- tibble::as_tibble(get.opp.table(db))
  opp <- opp[opp$quantile == quantile_, ]
  opp$date <- as.POSIXct(opp$date, format="%FT%T", tz="UTC")
  
  ## merge all metadata
  meta <- tibble::as_tibble(merge(sfl, opp, by="date"))
  meta <- meta %>% dplyr::select(
    date, lat, lon, conductivity, salinity, ocean_tmp, par, stream_pressure,
    event_rate, volume, all_count, opp_count, evt_count, opp_evt_ratio
  )
  
  return(meta)
}

#' Create a population data tibble for one quantile from the VCT
#'
#' @param db popcycle database file.
#' @param quantile OPP filtering quantile to use.
#' @return A tibble of realtime population data
#' @export
create_realtime_bio <- function(db, quantile_) {
  stat <- tibble::as_tibble(stat) %>%
    dplyr::mutate(date=as.POSIXct(time, format="%FT%T", tz="UTC")) %>%
    dplyr::filter(quantile == quantile_) %>%
    dplyr::select(date, pop, n_count, fsc_med, chl_med, pe_med, diam_mid_med, Qc_mid_med)
  
  return(stat)
}
