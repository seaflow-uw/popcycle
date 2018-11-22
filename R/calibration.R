#' Calculation of flow rate based on stream pressure measured by SeaFlow and the ratio of the detectable region
#'
#' @param  a vector that contains stream pressure measured by SeaFlow (available in SFL table and STAT table).
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with flow rate estimates
#' @export
flowrate <- function(stream_pressure, inst=inst){

  load(system.file('flowrate', paste0('lm_',inst),package='popcycle'))
    fr <- predict(reg, newdata=data.frame(measured.pressure=log10(stream_pressure)),interval='predict')

    # ratio of the volume of the stream analyzed by the laser (aka, detectable region) to the whole water stream (200 µm nozzle) for that instrument
    drr <- read.csv(system.file('flowrate', 'detectable_region.csv',package='popcycle'))
    drr.mean <- mean(drr[which(drr$seaflow_serial == inst), 'detectable_region_ratio'])
    drr.sd <- sd(drr[which(drr$seaflow_serial == inst), 'detectable_region_ratio'])

    flow_rate <- drr.mean * 10^fr[,'fit']  # mL min-1
    flow_rate.sd <- flow_rate * sqrt((log(10) * matrixStats::rowSds(fr))^2 + (drr.sd /drr.mean)^2) # uncertainties Antilog, base 10 : y=10^a so Sy= log(10) * y * Sa

    return(data.frame(cbind(flow_rate, flow_rate.sd)))
}




#' Estimate cell diameter and carbon cell quotas and biomass of phytoplankton populations based on normalized foward scatter to 1-µm beads used as internal standard
#'
#' @param opp Table that contains fsc_small values (transformed data).
#' @param beads.fsc Values of the fsc_small for beads (transformed data).
#' @param inst Instrument serial number
#' @return A dataframe with cell size and carbon cell quotas
#' @export
size.carbon.conversion <- function(opp, beads.fsc, inst){

  mie <- read.csv(system.file('scatter', paste0('calibrated-mie.csv'),package='popcycle'))

    # find closest matches in Mie lookup table
    id <- findInterval(opp[,'fsc_small']/as.numeric(beads.fsc), mie$scatter)

    #convert scatter to diameter and Qc
    for(quant in c('_lwr','_mid','_upr')){

      opp[,paste0('diam',quant)] <- mie[id,paste0('diam_',inst,quant)]
      opp[,paste0('Qc',quant)] <- mie[id,paste0('Qc_',inst,quant)]

    }

  return(opp)
}

#' Convert fsc_small values to cell diameter (diam, µm) and carbon quotas (Qc, pg)
#'
#' Convert fsc_small values to cell diameter (diam, µm) and carbon quotas (Qc, pg). Save per particle values
#' to SQLite3 database in vct.dir.
#'
#' @param db SQLite3 database file path.
#' @param opp.dir OPP file directory.
#' @param opp.files List of OPP files to classify. Include julian day directory.
#' @param vct.dir VCT file output directory.
#' @return Cell diameter (diam, µm) and carbon quotas (Qc, pg) for each particle
#' @examples
#' \dontrun{
#' convert.opp.files(db, opp.dir, opp.files, vct.dir)
#' }
#' @export
convert.opp.files <- function(db, opp.dir, opp.files, vct.dir){

  # Always assume the latest filter parameters are the ones used to generate
  # the current OPP data
  filter.params <- get.filter.params.latest(db)
  if (is.null(filter.params)) {
    stop('No DB filter entries')
  }

  # Get instrument serial number
  inst <- get.inst(db)

  i <- 0
  errors <- list()
  for (opp.file in opp.files) {
    message(round(100*i/length(opp.files)), '% completed \r', appendLF=FALSE)

    # delete old vct entries if they exist so we keep file/particle distinct
    # There should only be one vct entry in the db for each population/file
    # combination.
    delete.vct.by.file(vct.dir, opp.file)

    tryCatch({
      for (quantile in QUANTILES) {
        #print(paste('Loading', opp.file))
        opp <- get.opp.by.file(opp.dir, opp.file, quantile, channel='fsc_small',vct.dir=vct.dir)

        beads.fsc <- transformData(data.frame(fsc=filter.params[which(filter.params$quantile == quantile),'beads.fsc.small']))
        #print(paste('Classifying', opp.file))
        opp <- size.carbon.conversion(opp, beads.fsc=beads.fsc, inst=inst)

        # store vct
        vct <- opp[ , !(names(opp) %in% EVT.HEADER)]
        #print('Uploading labels to the database')
        save.vct.file(vct, vct.dir, opp.file, quantile)
      }
    }, error = function(e) {
      cat(paste0('Error with file ', opp.file, ': ', e))
    })

    i <- i + 1
    flush.console()
  }
}
