
#' Create a metadata tibble for one quantile appopriate for realtime analysis
#'
#' @param db popcycle database file.
#' @param quantile OPP filtering quantile to use.
#' @param volume Use a constant volume value, overriding any calculated values.
#' @return A tibble of realtime SFL and OPP table data
#' @export
create_realtime_meta <- function(db, quantile, volume = NULL) {
  quantile <- as.numeric(quantile)

  ## merge all metadata
  meta <- get_opp_table(db, sfl_join = TRUE, all_sfl_columns = TRUE, outlier_join = FALSE)
  meta <- meta[meta$quantile == quantile, ]
  # retrieve flow rate (mL min-1) of detectable volume
  fr <- flowrate(meta$stream_pressure, inst = get_inst(db))$flow_rate
  # convert to microL min-1
  fr <- fr * 1000
  # acquisition time (min)
  acq.time <- meta$file_duration / 60
  if (is.null(volume)) {
    # volume in microL
    meta$volume <- round(fr * acq.time, 0)
  } else {
    meta$volume <- volume
  }
  meta <- meta %>% dplyr::select(
    date, lat, lon, conductivity, salinity, ocean_tmp, par, stream_pressure,
    event_rate, volume, all_count, opp_count, evt_count, opp_evt_ratio
  )

  return(meta)
}

#' Write SFL metadata as a TSDATA file
#'
#' @param meta SFL metadata dataframe created by create_realtime_meta()
#' @param project Project identifier
#' @param outfile Output file path
#' @param filetype Filetype identifier
#' @param description Long form description of this file
#' @export
write_realtime_meta_tsdata <- function(meta, project, outfile, filetype = "SeaFlowSFL", description = "SeaFlow SFL data") {
  meta <- meta %>% dplyr::rename(time = date)
  fh <- file(outfile, open = "wt")
  writeLines(filetype, fh)
  writeLines(project, fh)
  writeLines(description, fh)
  writeLines(paste("ISO8601 timestamp", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", sep = "\t"), fh)
  writeLines(paste("time", "float", "float", "float", "float", "float", "float", "float", "float", "float", "integer", "integer", "integer", "float", sep = "\t"), fh)
  writeLines(paste("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", sep = "\t"), fh)
  close(fh)
  readr::write_delim(meta, outfile, delim = "\t", col_names = TRUE, append = TRUE)
}

#' Create a population data tibble for one quantile from the VCT
#'
#' @param db popcycle database file.
#' @param quantile OPP filtering quantile to use.
#' @param with_abundance Include volume normalized "abundance" column
#' @return A tibble of realtime population data
#' @export
create_realtime_bio <- function(db, quantile, correction = NULL, with_abundance = FALSE) {
  bio <- tibble::as_tibble(popcycle::get_stat_table(db)) %>%
    dplyr::mutate(date = lubridate::ymd_hms(time)) %>%
    dplyr::filter(quantile == {{ quantile }}) %>%
    dplyr::select(date, pop, n_count, abundance, diam_mid_med, diam_lwr_med) %>%
    dplyr::rename(diam_mid = diam_mid_med, diam_lwr = diam_lwr_med) %>%
    dplyr::mutate(correction = {{ correction }})

  if (! with_abundance) {
    bio <- bio %>% dplyr::select(-c("abundance"))
  }
  return(bio)
}

#' Write population data as a TSDATA file
#'
#' @param bio Population dataframe created by create_realtime_bio()
#' @param project Project identifier
#' @param outfile Output file path
#' @param filetype Filetype identifier
#' @param description Long form description of this file
#' @export
write_realtime_bio_tsdata <- function(bio, project, outfile,
                                      filetype = "SeaFlowPop",
                                      description = "SeaFlow population data") {
  bio <- bio %>% dplyr::rename(time = date)
  fh <- file(outfile, open="wt")
  writeLines(filetype, fh)
  writeLines(project, fh)
  writeLines(description, fh)
  if ("abundance" %in% colnames(bio)) {
    writeLines(paste("ISO8601 timestamp", "NA", "NA", "NA", "NA", "NA", "NA", sep = "\t"), fh)
    writeLines(paste("time", "category", "integer", "float", "float", "float", "float", sep = "\t"), fh)
    writeLines(paste("NA", "NA", "NA", "NA", "NA", "NA", "NA", sep = "\t"), fh)
  } else {
    writeLines(paste("ISO8601 timestamp", "NA", "NA", "NA", "NA", "NA", sep = "\t"), fh)
    writeLines(paste("time", "category", "integer", "float", "float", "float", sep = "\t"), fh)
    writeLines(paste("NA", "NA", "NA", "NA", "NA", "NA", sep = "\t"), fh)
  }
  close(fh)
  readr::write_delim(bio, outfile, delim = "\t", col_names = TRUE, append = TRUE)
}
