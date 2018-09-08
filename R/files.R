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

  if(length(id) == 0){
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



#' Calculation of flow rate based on stream pressure measured by SeaFlow and the
#'
#' @param dataframe that contains stream pressure measured by SeaFlow (available in SFL table and STAT table). Note that the dataframe needs to have a column named 'stream_pressure'
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with flow rate estimates
#' @export
flowrate <- function(stat, inst=inst){

  load(system.file("flowrate", paste0("lm_",inst),package='popcycle'))
    fr <- predict(reg, newdata=data.frame(measured.pressure=log10(stat$stream_pressure)),interval='predict')

    # ratio of the volume of the stream analyzed by the laser (aka, detectable region) to the whole water stream (200 µm nozzle) for that instrument
    drr <- read.csv(system.file("flowrate", "detectable_region.csv",package='popcycle'))
    drr.mean <- mean(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])
    drr.sd <- sd(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])

    stat$flow_rate <- drr.mean * 10^fr[,"fit"]  # mL min-1
    stat$flow_rate.sd <- stat$flow_rate * sqrt((log(10) * apply(fr, 1, sd))^2 + (drr.sd /drr.mean)^2) # uncertainties Antilog, base 10 : y=10^a so Sy= log(10) * y * Sa

    return(stat)

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

  if(is.null(inst)) inst <- get.meta(db)[2]

  stat <- get.raw.stat.table(db)
  stat <- flowrate(stat, inst=inst)

  # abundance is calculated based on a median value of opp_evt ratio for the entire cruise (volume of virtual core set for an entire cruise)
  stat[,c("abundance")]  <- stat[,"n_count"] / (1000* median(stat[,"opp_evt_ratio"], na.rm=T) * stat[,"flow_rate"] * stat[,"file_duration"]/60)   # cells µL-1
  stat[,c("abundance.sd")]  <- stat[,"abundance"] * stat[,"flow_rate.sd"] / stat[,"flow_rate"]           # cells µL-1

  # If Prochlorococcus present, abundance is calculated based on individual opp_evt ratio (each file), since it provides more accurate results (see https://github.com/armbrustlab/seaflow-virtualcore)
    id <- which(stat$pop == 'prochloro')
    if(length(id) > 0){
      stat[id,c("abundance")]  <- stat[id,"n_count"] / (1000* stat[id,"opp_evt_ratio"] * stat[id,"flow_rate"] * stat[id,"file_duration"]/60)   # cells µL-1
      stat[id,c("abundance.sd")]  <- stat[id,"abundance"] * stat[id,"flow_rate.sd"] / stat[id,"flow_rate"]           # cells µL-1
    }

  return(stat)

}

#' Normalization by beads used as internal standard
#'
#' @param stat Stat table generated by the function get.stat.table().
#' @param spar smooothing parameter, the higher the more smoothing is applied.
#' @return A dataframe with normalized D1, D2, fsc_small, chl_small, pe and fsc_perp values
#' @export
normalization <- function(stat, spar=0.7){

  #check that there is beads in the table
  if(!any(unique(stat$pop) == 'beads')){
    print("no beads found, normalization can't be done")
    stop
  }

  time <- as.POSIXct(stat$time, format = "%FT%T", tz = "GMT")
  beads <- subset(stat, pop =='beads' & quantile== 50)
    beads$time <- as.POSIXct(beads$time, format = "%FT%T", tz = "GMT")

    channels <- c("D1","D2","fsc_small","chl_small","pe","fsc_perp")

  par(mfrow=c(3,2),cex=1.2, mar=c(2,5,1,1), oma=c(1,1,1,1))

    for(para in channels){
          smooth <- smooth.spline(beads$time, beads[,para],spar=spar)
          smooth.beads <- spline(smooth$x, smooth$y, xout=unique(time))
          plot(beads$time, beads[,para], ylim=c(1, 10^3.5), log='y', xlab=NA, ylab=paste(para))
            lines(smooth.beads, col=2, lwd=3)
          id <- findInterval(time,beads$time)
          stat[,para] <- stat[,para] / smooth.beads$y[id]
          }

  return(stat)

}



#' Estimate Carbon cell quotas and biomass based on normalized foward scatter to 1-µm beads used as internal standard
#'
#' @param stat Table generated by the function normalization().
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with carbon cell quotas and biomass
#' @export
carbon_conversion <- function(stat, inst=NULL){

  if(is.null(inst)) inst <- get.meta(db)[2]

  load(system.file("cbiomass", paste0("lm_",inst),package='popcycle'))
  qc <- predict(reg, newdata=data.frame(norm.fsc=log10(stat$fsc_small)),interval='predict')

  stat[,"Qc"] <- 10^qc[,"fit"] * 1000        # fgC cell-1
  stat[,"Qc.sd"] <-   log(10) * stat[,"Qc"] * apply(qc, 1, sd) # uncertainties Antilog, base 10 : y=10^a so Sy= log(10) * y * Sa
  stat[,"Cbiomass"] <- stat[,"Qc"] * stat[,"abundance"] / 1000      # pgC L-1
  stat[,"Cbiomass.sd"] <- stat[,"Cbiomass"] * sqrt((stat[,"abundance.sd"] / stat[,"abundance"])^2 + (stat[,"Qc.sd"] / stat[,"Qc"])^2) # pgC L-1

  return(stat)

}

#' Calculate mean  + sd of aggregate statistics data from the 3 OPP data sets (2.5, 50 and 97.5 qunatile).
#'
#' @param stat. Table generated by the function carbon_conversion.
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' DF <- merge.stat(stat)
#' }
#' @export
merge.stat <- function(stat){

    para <- c("abundance", "Qc","Cbiomass")
    lwr <- upr <- stat
      for(p in para) lwr[,p] <- stat[,p]-stat[,paste0(p,".sd")]
      for(p in para) upr[,p] <- stat[,p]+stat[,paste0(p,".sd")]
      df <- rbind(lwr,upr)

    para <- colnames(stat)[-c(1,2,13,21,23,25,27,29)]
      df.mean <- aggregate(df[,para], by=list(time=df$time, population=df$pop), FUN=mean)
      df.mean$file <- stat[match(df.mean$time, stat$time),"file"]

    para <- c("D1","D2","fsc_small","chl_small","pe","fsc_perp","abundance", "Qc","Cbiomass")
      df.sd <- aggregate(df[,para], by=list(time=df$time, population=df$pop), FUN=sd)[,-c(1,2)]
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
#' @param spar smooothing parameter, the higher the more smoothing is applied.
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' stat <- get.clean.stat.table(db, inst=NULL, spar=0.7)
#' }
#' @export
get.clean.stat.table <- function(db, inst=NULL, spar=0.7){

  print("1. Getting raw data"); stat <- get.stat.table(db, inst=inst);
  print("2. Normalizing channel values using beads"); stat <- normalization(stat, spar=spar)
  print("3. Converting normalized light scattering to carbon"); stat <- carbon_conversion(stat, inst=inst);
  print("4. Propagating error"); stat <- merge.stat(stat)

  outlier <- get.outlier.table(db)
  stat <- merge(stat, outlier, all.x=TRUE)

  return(stat)

}
