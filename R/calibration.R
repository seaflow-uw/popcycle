#' Calculation of flow rate based on stream pressure measured by SeaFlow and the ratio of the detectable region
#'
#' @param  a vector that contains stream pressure measured by SeaFlow (available in SFL table and STAT table).
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with flow rate estimates
#' @export
flowrate <- function(stream_pressure, inst=inst){

  load(system.file("flowrate", paste0("lm_",inst),package="popcycle"))
    fr <- predict(reg, newdata=data.frame(measured.pressure=log10(stream_pressure)),interval="predict")

    # ratio of the volume of the stream analyzed by the laser (aka, detectable region) to the whole water stream (200 µm nozzle) for that instrument
    drr <- read.csv(system.file("flowrate", "detectable_region.csv",package="popcycle"))
    drr.mean <- mean(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])
    drr.sd <- sd(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])

    flow_rate <- drr.mean * 10^fr[,"fit"]  # mL min-1
    flow_rate.sd <- flow_rate * sqrt((log(10) * matrixStats::rowSds(fr))^2 + (drr.sd /drr.mean)^2) # uncertainties Antilog, base 10 : y=10^a so Sy= log(10) * y * Sa

    return(data.frame(cbind(flow_rate, flow_rate.sd)))
}




#' Estimate cell diameter and carbon cell quotas and biomass of phytoplankton populations based on normalized forward scatter to 1-µm beads used as internal standard
#'
#' @param opp Table that contains fsc_small values (transformed data).
#' @param beads_fsc Values of the fsc_small for beads (transformed data).
#' @param inst Instrument serial number
#' @param mie Optional Mie theory lookup dataframe
#' @return A dataframe with cell size and carbon cell quotas
#' @export
size_carbon_conversion <- function(opp, beads_fsc, inst, mie = NULL){
    was_tibble <- FALSE
    if ("tbl_df" %in% class(opp)) {
      opp <- as.data.frame(opp)
    }
    if (is.null(mie)) {
        mie <- read_mie_csv()
    }

    # find closest matches in Mie lookup table
    id <- findInterval(opp[,"fsc_small"]/as.numeric(beads_fsc), mie$scatter, all.inside = TRUE)

    #convert scatter to diameter and Qc
    for(quant in c("_lwr","_mid","_upr")){

      opp[,paste0("diam",quant)] <- mie[id,paste0("diam_",inst,quant)]
      opp[,paste0("Qc",quant)] <- mie[id,paste0("Qc_",inst,quant)]

    }

    if (was_tibble) {
      opp <- tibble::as_tibble(opp)
    }

  return(opp)
}

#' Calibrate abundance based on Influx data
#'
#' @param stat The stat table returned from get_stat_table()
#' @param cruisename Name of the cruise (returned from get_metadata_table()).
#' @param calib Optional calibration calibration  dataframe of regression values (cruise=cruise.name, pop=prochloro, a=slope, b=intercept)
#' @return the stat table witrh corrected abundancce if available
#' @export
stat_calibration <- function(stat, cruisename, calib=NULL){

    if (is.null(calib)) {
        calib <- read_calib_csv()
    }

  # If Prochlorococcus and Synechococus present, abundance is corrected based on abundance measured by Influx
  for(phyto in c("prochloro", "synecho")){
    corr <- calib %>% dplyr::filter(cruise == cruisename & pop == phyto)
    if(nrow(corr) > 0){ 
      print(paste("Calibrated abundance for", phyto))
      id <- which(stat$pop == phyto)
      stat[id,c("abundance")]  <- stat[id,c("abundance")] * corr$a + corr$b  # cells µL-1
    }
  }  
  
  return(stat)
}



#' Calibrate abundance based on Influx data
#'
#' @param PSD Particle size disitribution created by create_PSD().
#' @param cruisename Name of the cruise (returned from get_metadata_table()).
#' @param calib Optional calibration calibration  dataframe of regression values (cruise=cruise.name, pop=prochloro, a=slope, b=intercept)
#' @return the stat table with corrected abundancce if available
#' @export
PSD_calibration <- function(PSD, cruisename, calib=NULL){

  if (is.null(calib)) {
        calib <- read_calib_csv()
  }
  # select column that have PSD data
  clmn <- grep("]", names(PSD))

  # If Prochlorococcus and Synechococus present, abundance is corrected based on abundance measured by Influx
  for(phyto in c("prochloro", "synecho")){
    corr <- calib %>% dplyr::filter(cruise == cruisename & pop == phyto)
    if(nrow(corr) > 0){ 
      print(paste("Calibrated abundance for", phyto))
      id <- which(PSD$pop == phyto)
      PSD[id,clmn] <- PSD[id,clmn] * corr$a + (PSD[id,clmn] / rowSums(PSD[id,clmn])) * corr$b  # cells µL-1
    }
  }

  return(PSD)
}
