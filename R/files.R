#' Find EVT files with a recursive search down a directory tree.
#'
#' @param evt.dir Directory containing EVT files.
#' @return Vector of EVT files with julian day directory.
#' @examples
#' \dontrun{
#' evt.files <- get.evt.files(evt.dir)
#' }
#' @export
get.evt.files <- function(evt.dir) {
  file.list <- list.files(evt.dir, recursive=T)
  if (length(file.list) == 0) {
    print(paste("no evt files found in", evt.dir))
    return(file.list)
  }
  # regexp to match both types of EVT files
  #   - 37.evt (old style)
  #   - 2014-05-15T17-07-08+0000 or 2014-07-04T00-03-02+00-00 (new style)
  # In the new style the final timezone offset may not always be UTC (00-00)
  # so be sure to correctly parse it in all code.
  regexp <- "/?[0-9]+\\.evt(\\.gz)?$|/?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}[+-][0-9]{2}-?[0-9]{2}(\\.gz)?$"
  id <- grep(regexp,file.list)
  file.list <- file.list[id]
  #print(paste(length(file.list), "evt files found"))
  return(sort(unlist(lapply(file.list, clean.file.path))))
}

#' Find the most recent EVT file.
#'
#' @param evt.dir Directory containing EVT files.
#' @return Most recent EVT file with julian day directory.
#' @examples
#' \dontrun{
#' evt.file <- get.latest.evt(evt.dir)
#' }
#' @export
get.latest.evt <- function(evt.dir) {
  file.list <- get.evt.files(evt.dir)
  n <- length(file.list)
  if (n == 0) {
    return(NA)
  }
  return(clean.file.path(file.list[n]))
}

#' file.transfer
#'
#' @return None
#' @export
file.transfer <- function(evt.dir, instrument.dir){

  last.evt <- get.latest.evt(evt.dir)
  file.list <- list.files(instrument.dir, recursive=T)
  sfl.list <- file.list[grepl('.sfl', file.list)]
  file.list <- file.list[-length(file.list)] # remove the last file (opened file)
  file.list <- sort(file.list[!grepl('.sfl', file.list)])

  id <- match(last.evt, file.list)

  if(is.na(id)){
    day <- unique(dirname(file.list))
      for(d in day) system(paste0("mkdir ",evt.dir,"/",d))
    print(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list))
    system(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list, collapse=";"))
    system(paste0("scp ",instrument.dir,"/",sfl.list," ", evt.dir,"/",sfl.list, collapse=";"))
  }
  else{
    file.list <- file.list[id:length(file.list)]
    day <- unique(dirname(file.list))
      for(d in day) system(paste0("mkdir ",evt.dir,"/",d))
    print(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list))
    system(paste0("scp ",instrument.dir,"/",file.list," ", evt.dir,"/",file.list, collapse=";"))
    system(paste0("scp ",instrument.dir,"/",sfl.list," ", evt.dir,"/",sfl.list, collapse=";"))
  }
 }

#' Clean a file path.
#'
#' Convert an EVT/OPP/VCT file path to a form suitable for storage in the SQLite
#' db. Any ".gz", ".opp", ".vct" extensions will be removed.
#'
#' @param fpath File path to clean.
#' @return Modified file path as julian_day/EVT_file_name.
#' @examples
#' \dontrun{
#' fpath <- clean.file.path("foo/2014_185/2014-07-04T00-00-02+00-00.opp.gz")
#' }
#' @export
clean.file.path <- function(fpath) {
  # Clean up any places with multiple "/"s
  fpath <- gsub("/+", "/", fpath)

  # Check for julian day directory
  parts <- unlist(strsplit(fpath, "/"))
  if (length(parts) < 2) {
    stop(paste0("file path ", fpath, " must contain a julian day directory"))
  }

  file.name <- parts[length(parts)]
  julian.day <- parts[length(parts)-1]

  julian.regexp <- "^[0-9]{4}_[0-9]+$"
  if (length(grep(julian.regexp, julian.day)) != 1) {
    stop(paste0("Julian day directory does not match pattern YYYY_day in ", fpath))
  }

  # Get rid of any .gz extension
  if (nchar(file.name) >= 3) {
    if (substr(file.name, nchar(file.name) - 2, nchar(file.name)) == ".gz") {
      file.name <- substr(file.name, 1, nchar(file.name) - 3)
    }
  }

  # Get rid of any .opp extension
  if (nchar(file.name) >= 4) {
    if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) == ".opp") {
      file.name <- substr(file.name, 1, nchar(file.name) - 4)
    }
  }

  # Get rid of any .vct extension
  if (nchar(file.name) >= 4) {
    if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) == ".vct") {
      file.name <- substr(file.name, 1, nchar(file.name) - 4)
    }
  }

  return(paste(julian.day, file.name, sep="/"))
}

#' Check if file path ends with suffix
#'
#' @param path File path to test.
#' @param ending String suffix to test
#' @return TRUE or FALSE if path ends with ending. If either path or ending has
#'   length zero or if ending is longer than path, return FALSE.
#' @examples
#' \dontrun{
#' endswith("foo/bar.txt", ".txt") # TRUE
#' endswith("foo/bar.txt", ".gz")  # FALSE
#' }
#' @export
endswith <- function(path, ending) {
  psize <- nchar(path)
  esize <- nchar(ending)
  if (psize > 0 && esize > 0 && psize >= esize) {
    return(substr(path, psize - esize + 1, psize) == ending)
  }
  return(FALSE)
}



#' Extract dawn and dusk times from a time series of PAR values
#'
#' Extracts the time that dawn and dusk occur from a time series consisting of PAR values (light intensity).
#' Dawn is defined as the point where the par value goes from night -> day.
#' Dusk is defined as the point where the par value goes from day -> night.
#' This is useful for gating purposes for flow- cytometry.
#' Dawn and dusk tend to represent the extrema of flourences for phytoplankton.
#' @param x A data frame with two columns. The first column must be time values
#'    in as.POSIXct format. The second column must be PAR values.
#' @param cutoff An integer representing the smallest par value that is considered
#'  daytime.
#' @return A vector consisting of dawn and dusk time values in as.POSIXct format
#' @examples
#' \dontrun{
#' par.data.csv <- system.file("extdata/par_data.csv", package="popcycle")
#' par.data <- read.csv(par.data.csv)
#' par.data$date <- as.POSIXct(par.data$date, format = "%FT%T", tz = "GMT")
#' dawn.dusk.times <- get.dawn.dusk.time(par.data, 10)
#' }
#' @export
get.dawn.dusk.time <- function(x, cutoff) {
    # assign names to the time and par parameters
    time <- x[,1]
    par <- x[,2]

    # We will define dawn and dusk times to occur when par goes from above 10 -> below 10,
    # or from below 10 -> above 10
    above <- par > cutoff
    intersect <- which(diff(above)!=0)

    # We will then make sure we are only getting points that fall within the sequential pattern
    # of natural dusk and dawns, AKA they must be separated by same time intervals

    #revision <- intersect[which(diff(intersect) > mean(diff(intersect)))]
    #dawn.dusk <- time[revision]

    dawn.dusk <- time[intersect]
    return(dawn.dusk)
}




#' Concatenate EVT or OPP files
#'
#' @param evtopp.list List of EVT or OPP files (full path required).
#' @param n Number of rows to return.
#' @param min.fsc, min.pe, min.chl Minimum value for fsc_small, pe and chl_small respectively
#' @return A dataframe with n rows.
#' @export
concatenate.evtopp <- function(evtopp.list, n=100000, min.fsc=0, min.pe=0, min.chl=0, transform=TRUE,...){
  n <- as.numeric(n)
  DF <- NULL
  i <- 0
  for (file in evtopp.list){
        message(round(100*i/length(evtopp.list)), "% completed \r", appendLF=FALSE)

        tryCatch({
          df <- readSeaflow(file,transform=transform,...)
          df <- subset(df, fsc_small > min.fsc & pe > min.pe & chl_small > min.chl)
          df <- df[round(seq(1,nrow(df), length.out=round(n/length(evtopp.list)))),]

            if(any(is.na(df))) next
            DF <- rbind(DF, df)
            }, error = function(e) {
              cat(paste0("Error with file ", file, ": ", e))
          })

          i <- i + 1
          flush.console()
          }

      return(DF)
}



#' Calculation of flow rate based on stream pressure measured by SeaFlow and the ratio of the detectable region
#'
#' @param  a vector that contains stream pressure measured by SeaFlow (available in SFL table and STAT table).
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with flow rate estimates
#' @export
flowrate <- function(stream_pressure, inst=inst){

  load(system.file("flowrate", paste0("lm_",inst),package='popcycle'))
    fr <- predict(reg, newdata=data.frame(measured.pressure=log10(stream_pressure)),interval='predict')

    # ratio of the volume of the stream analyzed by the laser (aka, detectable region) to the whole water stream (200 µm nozzle) for that instrument
    drr <- read.csv(system.file("flowrate", "detectable_region.csv",package='popcycle'))
    drr.mean <- mean(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])
    drr.sd <- sd(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])

    flow_rate <- drr.mean * 10^fr[,"fit"]  # mL min-1
    flow_rate.sd <- flow_rate * sqrt((log(10) * apply(fr, 1, sd))^2 + (drr.sd /drr.mean)^2) # uncertainties Antilog, base 10 : y=10^a so Sy= log(10) * y * Sa

    return(data.frame(cbind(flow_rate, flow_rate.sd)))

}


#' Get aggregate statistics data frame along with estimates of cell abundance.
#'
#' @param db SQLite3 database file path.
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' stats <- get.stat.table(db, inst=NULL)
#' }
#' @export
get.stat.table <- function(db, inst=NULL) {

  if (is.null(inst)) {
    inst <- get.inst(db)
  }

  stat <- get.raw.stat.table(db)
  fr <- flowrate(stat$stream_pressure, inst=inst)

  stat[,"flow_rate"] <- fr[,1]
  stat[,"flow_rate.sd"] <- fr[,2]

  # abundance is calculated based on a median value of opp_evt ratio for the entire cruise (volume of virtual core set for an entire cruise)
  stat[,c("abundance")]  <- stat[,"n_count"] / (1000* median(stat[,"opp_evt_ratio"], na.rm=T) * stat[,"flow_rate"] * stat[,"file_duration"]/60)   # cells µL-1
  stat[,c("abundance.sd")]  <- stat[,"abundance"] * stat[,"flow_rate.sd"] / stat[,"flow_rate"]           # cells µL-1

  # If Prochlorococcus present, abundance is calculated based on individual opp_evt ratio (each file), since it provides more accurate results (see https://github.com/armbrustlab/seaflow-virtualcore)
    id <- which(stat$pop == 'prochloro' | stat$pop == 'synecho')
    if(length(id) > 0){
      stat[id,c("abundance")]  <- stat[id,"n_count"] / (1000* stat[id,"opp_evt_ratio"] * stat[id,"flow_rate"] * stat[id,"file_duration"]/60)   # cells µL-1
      stat[id,c("abundance.sd")]  <- stat[id,"abundance"] * stat[id,"flow_rate.sd"] / stat[id,"flow_rate"]           # cells µL-1
    }

  return(stat)

}

#' Normalization by beads used as internal standard
#'
#' @param stat Stat table generated by the function get.stat.table().
#' @param channel Channel (fsc_small by default) to be normalized by beads (used as internal standard).
#' @param norm Value of beads signal used for the normalization. If NULL (default), median value of beads of the lower quantile from the Stat table will be used as input.
#' @return A dataframe with normalized channel values
#' @export
normalization <- function(stat, channel="fsc_small", norm=NULL){

  stat[,paste0("norm.",channel)] <- NA

  #check that there are beads in the Stat table
  if(is.null(norm)){
            if(!any(unique(stat$pop) == 'beads')){
              print("no beads values provided, normalization can't be done!")
              stop
            }else{
        beads <- subset(stat, pop=='beads' & quantile == 2.5)
        norm <- median(beads[,channel])
      }
    }

  stat[,paste0("norm.",channel)] <- stat[,channel] / norm

return(stat)

}


#' Estimate cell diameter and carbon cell quotas and biomass of phytoplankton populations based on normalized foward scatter to 1-µm beads used as internal standard
#'
#' @param stat Table that contains fsc_small values normalized by 1 micron beads.
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with cell size, carbon cell quotas and biomass
#' @export
size_carbon_conversion <- function(stat, inst=NULL){

  if(!any(colnames(stat) == "norm.fsc_small")){
              print("no normalized fsc_small values, conversion can't be done!")
              stop
            }

  if (is.null(inst)) {
    inst <- get.inst(db)
  }

  mie <- read.csv(system.file("scatter", paste0("calibrated-mie.csv"),package='popcycle'))
  u <- approx(x=log10(mie[,paste0("scatter_",inst)]), y=log10(mie$Qc), n=10^3.5)
  v <- approx(x=log10(mie[,paste0("scatter_",inst)]), y=log10(mie$diameter), n=10^3.5)

  id.u <- findInterval(log10(stat[,"norm.fsc_small"]), u$x)
  id.v <- findInterval(log10(stat[,"norm.fsc_small"]), v$x)

  #diameter (µm)
  stat[,"diameter"] <- 10^v$y[id.v]
  stat[,"diameter.sd"] <-  stat[,"diameter"] * 0.3 # TO DO, use uncertainities in Mie prediction to calcualte diamter.sd

  # carbon cell quotas (fgC cell-1)
  stat[,"Qc"] <- 2* 10^u$y[id.u] * 1000
  stat[,"Qc.sd"] <-  stat[,"Qc"] * 0.3 # TO DO, use uncertainities in Mie prediction to calcualte Qc.sd

  # carbon biomass (pgC L-1)
  stat[,"Cbiomass"] <- stat[,"Qc"] * stat[,"abundance"] / 1000
  stat[,"Cbiomass.sd"] <- stat[,"Cbiomass"] * sqrt((stat[,"abundance.sd"] / stat[,"abundance"])^2 + (stat[,"Qc.sd"] / stat[,"Qc"])^2)
  return(stat)

}

#' Calculate mean  + sd of aggregate statistics data from the 3 OPP data sets (2.5, 50 and 97.5 qunatile).
#'
#' @param stat Table generated by the function carbon_conversion.
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' DF <- merge.stat(stat)
#' }
#' @export
merge.stat <- function(stat){

    para <- c("abundance", "diameter","Qc","Cbiomass")
    lwr <- upr <- mid <- stat
      for(p in para) lwr[,p] <- stat[,p]-stat[,paste0(p,".sd")]
      for(p in para) upr[,p] <- stat[,p]+stat[,paste0(p,".sd")]
      for(p in para) mid[,p] <- stat[,p]
      df <- rbind(lwr, mid, upr)

    para <- c("lat","lon","temp","salinity","conductivity","par","stream_pressure","file_duration","event_rate","opp_evt_ratio","n_count","D1","D2","fsc_small","chl_small","pe","fsc_perp","flow_rate","abundance","norm.fsc_small", "diameter","Qc","Cbiomass")
      df.mean <- aggregate(df[,para], by=list(time=df$time, pop=df$pop), FUN=mean)
      df.mean$file <- stat[match(df.mean$time, stat$time),"file"]

    para <- c("D1","D2","fsc_small","chl_small","pe","fsc_perp","abundance","norm.fsc_small","diameter", "Qc","Cbiomass")
      df.sd <- aggregate(df[,para], by=list(time=df$time, pop=df$pop), FUN=sd)[,-c(1,2)]
      colnames(df.sd) <- paste0(para,".sd")

    DF <- cbind(df.mean,df.sd)
    DF <- DF[order(DF$time),]

      return(DF)
}



#' Get aggregate statistics data frame along with propagation of errors
#'
#' @param db SQLite3 database file path.
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' stat <- get.clean.stat.table(db, inst=NULL, spar=0.7)
#' }
#' @export
get.clean.stat.table <- function(db, inst=NULL){

  if (is.null(inst)) {
    inst <- get.inst(db)
  }

  print("1. Compute aggregate statistic")
      stat <- get.stat.table(db, inst=inst)
  print("2. Normalizing channel values using beads")
      stat <- normalization(stat)
  print("3. Converting normalized light scattering to cell diameter & carbon quotas")
      stat <- size_carbon_conversion(stat, inst=inst)
  print("4. Merging quantiles")
      stat <- merge.stat(stat)

  outlier <- get.outlier.table(db)
  stat <- merge(stat, outlier, all.x=TRUE)

  return(stat)

}
