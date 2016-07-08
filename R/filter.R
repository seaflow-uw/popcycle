#' Filter EVT particles with a generic filter function.
#'
#' @param evt EVT data frame.
#' @param filter.func Filtering function.
#' @return Named list where list$params contains a list of filtering parameters
#'   and list$opp contains OPP data frame.
#' @examples
#' \dontrun{
#' filt <- filter.evt(evt, filter.notch, origin=NA, width=0.5, notch1=NA,
#'                    notch2=NA, offset=0)
#' }
#' @export
filter.evt <- function(evt, filter.func, ...) {
  filt <- filter.func(evt, ...)

  # SANITY CHECKS
  # need same columns for opp
  if (!all(names(evt) == names(filt$opp))) {
    stop('Filtering function produced OPP with different columns')
  }

  return (filt)
}

#' Filter EVT particles.
#'
#' @param evt EVT data frame.
#' @param origin,width,notch1,notch2,offset Filtering parameters. origin,
#'   notch1, and notch2 will be calculated if NA.
#' @return Named list where list$params contains a list of filtering parameters
#'   and list$opp contains OPP data frame.
#' @examples
#' \dontrun{
#' filt <- filter.notch(evt, origin=NA, width=0.5, notch1=NA, notch2=NA, offset=0)
#' }
#' @export
filter.notch <- function(evt, origin=NA, width=1.0, notch1=NA, notch2=NA, offset=0) {

  origin <- as.numeric(origin)
  width <- as.numeric(width)
  notch1 <- as.numeric(notch1)
  notch2 <- as.numeric(notch2)
  offset <- as.numeric(offset)

  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    return(list("opp"=data.frame(c())))
  }

  # linearize the LOG transformed data
  lin <- FALSE
  id <- which(colnames(evt) == "pulse_width" | colnames(evt) == "time" | colnames(evt) =="pop")
  if (!any(max(evt[,-c(id)]) > 10^3.5)) {
    evt <- .untransformData(evt)
    lin <- TRUE
  }

  # Filtering out noise
  evt. <- evt[which(evt$fsc_small > 1 & evt$D1 > 1 & evt$D2 > 1),]

  # Correction for the difference of sensitivity between D1 and D2
  if(is.na(origin))  origin <- median(evt.$D2-evt.$D1)

  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < (D1+origin) + width * 10^4 & (D1+origin) < D2 + width * 10^4)

  # finding the notch
  fsc.max <- max(aligned$fsc_small)
  if (is.na(notch1)) {
    d.min1 <- min(aligned[which(aligned$fsc_small == fsc.max),"D1"]) # find the best particle of the largest OPP
    notch1 <- fsc.max / (d.min1 - offset*10^4)
  }

  if (is.na(notch2)) {
    d.min2 <- min(aligned[which(aligned$fsc_small == fsc.max),"D2"])
    notch2 <- fsc.max / (d.min2 - offset*10^4)
  }

  # Filtering focused particles (fsc_small > D + notch)
  opp <- subset(aligned, fsc_small >= D1*notch1 - offset*10^4 & fsc_small >= D2*notch2 - offset*10^4)

  params = list("notch1"=notch1, "notch2"=notch2, "offset"=offset,
                "origin"=origin, "width"=width)
  return(list("opp"=opp, "params"=params))
}




#' Plot helpful cytograms for exploring filtering parameters.
#'
#' @param evt EVT data frame.
#' @param origin,width,notch1,notch2,offset Filtering parameters. origin,
#'   notch1, and notch2 will be calculated if NA.
#' @return None
#' @export
plot.filter.cytogram <- function(evt, origin=NA, width=1, notch1=NA, notch2=NA, offset=0) {
  origin <- as.numeric(origin)
  width <- as.numeric(width)

  notch1 <- as.numeric(notch1)
  notch2 <- as.numeric(notch2)
  offset <- as.numeric(offset)

  # linearize the LOG transformed data
  id <- which(colnames(evt) == "fsc_small" | colnames(evt) == "chl_small" | colnames(evt) =="pe" | colnames(evt) =="fsc_perp" | colnames(evt) =="D1" | colnames(evt) =="D2")
  if (!any(max(evt[,c(id)]) > 10^3.5)) {
    evt[,c(id)] <- (log10(evt[,c(id)])/3.5)*2^16
  }

  # Filtering out noise
  evt. <- subset(evt, evt$fsc_small > 1 & evt$D1 > 1 & evt$D2 > 1)

  # Correction for the difference of sensitivity between D1 and D2
  if (is.na(origin)) origin <- median(evt.$D2-evt.$D1)

  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < (D1+origin) + width * 10^4 & (D1+origin) < D2 + width * 10^4)

  # finding the notch
  fsc.max <- max(aligned$fsc_small)
  if (is.na(notch1)) {
    d.min1 <- min(aligned[which(aligned$fsc_small == fsc.max),"D1"]) # find the best particle of the largest OPP
    notch1 <- fsc.max / (d.min1 - offset*10^4)
  }

  if (is.na(notch2)) {
    d.min2 <- min(aligned[which(aligned$fsc_small == fsc.max),"D2"])
    notch2 <- fsc.max / (d.min2 - offset*10^4)
  }

  # Filtering focused particles (fsc_small > D * notch)
  opp <- subset(aligned, fsc_small >= D1*notch1 - offset*10^4 & fsc_small >= D2*notch2 - offset*10^4)

  ################
  ### PLOTTING ###
  ################
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  percent.opp <- round(100*nrow(opp)/nrow(evt.),2)

  origin1 <- origin + width*10^4
  origin2 <- origin - width*10^4

  if(nrow(evt) > 10000){
    evt <- evt[round(seq(1,nrow(evt), length.out=10000)),]
    evt. <- evt.[round(seq(1,nrow(evt.), length.out=10000)),]
      }
  if(nrow(aligned) > 10000)  aligned <- aligned[round(seq(1,nrow(aligned), length.out=10000)),]

  def.par <- par(no.readonly = TRUE) # save default, for resetting...

  par(mfrow=c(3,2),pty='s')

  plot.cytogram(evt, "D1", "D2")
  mtext("Noise", side=3, line=3, font=2, col=2)
  draw.circle(0,0, radius=2000, border=2, lwd=2)
  mtext("fsc_small=1 & D1=1 & D2=1", side=3, line=2, font=2)

  plot.cytogram(evt., "D1", "D2")
  mtext("Alignment", side=3, line=3, font=2, col=2)
  abline(b=1, a=origin1, col='red',lwd=2)
  abline(b=1, a=origin2, col='red',lwd=2)
  mtext(paste("D2 - D1=", round(origin,2)),side=3, line=2,font=2)
  mtext(paste("Width=", width),side=3, line=1,font=2)

  plot.cytogram(aligned, "fsc_small", "D1")
  mtext("Focus", side=3, line=3, font=2,col=2)
  mtext(paste("Notch 1=", round(notch1, 2)),side=3, line=2,font=2)
  mtext(paste("Offset=", offset),side=3, line=1,font=2)
  abline(b=1/notch1, a=offset*10^4, col=2,lwd=2)

  plot.cytogram(aligned, "fsc_small", "D2")
  mtext("Focus", side=3, line=3, font=2,col=2)
  mtext(paste("Notch 2=", round(notch2, 2)),side=3, line=2,font=2)
  mtext(paste("Offset=", offset),side=3, line=1,font=2)
  abline(b=1/notch2, a=offset*10^4, col=2,lwd=2)

  plot.cytogram(opp, "fsc_small", "pe")
  mtext("OPP", side=3, line=1, font=2, col=2)
  plot.cytogram(opp, "fsc_small","chl_small")
  mtext("OPP", side=3, line=1, font=2, col=2)
  mtext(paste("OPP =", percent.opp,"% EVT"), outer=T,side=1, line=-2,font=2,col=1)

  par(def.par)
}

#' Plot helpful cytograms for exploring filtering parameters for one file.
#'
#' @param evt.dir EVT file directory.
#' @param file.name File name with julian day directory.
#' @param origin,width,notch1,notch2,offset Filtering parameters. origin,
#'   notch1, and notch2 will be calculated if NA.
#' @return None
#' @export
plot.filter.cytogram.by.file <- function(evt.dir, file.name, origin=NA, width=0.5, notch1=NA, notch2=NA, offset=0) {
  file.name <- clean.file.path(file.name)
  evt <- readSeaflow(file.path(evt.dir, file.name))
  plot.filter.cytogram(evt, origin=origin, notch1=notch1, notch2=notch2,
		                   width=width, offset=offset)
}



#' find.filter.notch
#'
#' @export
find.filter.notch <- function(evt.list, origin=NA, width=1.0, offset=0, do.plot=TRUE){

  origin <- as.numeric(origin)
  width <- as.numeric(width)
  offset <- as.numeric(offset)


DF <- NULL

   for(file in evt.list){
      evt <- readSeaflow(file, transform=F)

      if(nrow(evt) == 0){
        print(paste("no evt found in", file))
        next
        }

      print(paste("processing ",file))

      # Filtering out noise
        evt. <- evt[which(evt$fsc_small > 1 & evt$D1 > 1 & evt$D2 > 1),]

     # Correction for the difference of sensitivity between D1 and D2
        if(is.na(origin)){
          origin. <- median(evt.$D2-evt.$D1)
          }else{origin. <- origin}

      # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
        aligned <- subset(evt., D2 < (D1+origin.) + width * 10^4 & (D1+origin.) < D2 + width * 10^4)

      # finding the notch
      fsc.max <- max(aligned$fsc_small)
      d.min1 <- min(aligned[which(aligned$fsc_small == fsc.max),"D1"]) # find the best particle of the largest OPP
      d.min2 <- min(aligned[which(aligned$fsc_small == fsc.max),"D2"])
      notch1 <- fsc.max / (d.min1 - offset*10^4)
      notch2 <- fsc.max / (d.min2 - offset*10^4)

      opp <- subset(aligned, fsc_small >= D1*notch1 - offset*10^4 & fsc_small >= D2*notch2 - offset*10^4)

      if(nrow(opp) == 0){
        print(paste("no opp found in", file))
        next
        }

      para <- data.frame(cbind(file=file, notch1=as.numeric(round(notch.1,1)), notch2=as.numeric(round(notch.2,1)),
                    origin=as.numeric(round(origin.,1),0), fsc.max=as.numeric(round(max(opp$fsc_small))), chl.max=as.numeric(round(max(opp$chl_small))),
                    fsc.med=as.numeric(round(median(opp$fsc_small))), chl.med=as.numeric(round(median(opp$chl_small))),
                    original=as.numeric(nrow(evt)), passed=as.numeric(nrow(opp))),stringsAsFactors = FALSE)

       DF <- rbind(DF, para)
      }


     if(do.plot){
    def.par <- par(no.readonly = TRUE) # save default, for resetting...

      mean.notch1 <- median(as.numeric(DF$notch1), na.rm=T)
      mean.notch2 <- median(as.numeric(DF$notch2), na.rm=T)


    par(mfrow=c(3,1),oma=c(2,2,2,4), cex=1)
    par(pty='s')
    plot(DF[,c('notch1', 'notch2')], asp=1)
      abline(b=1, a=0, col="grey", lty=2)
    points(x=mean.notch1, y=mean.notch2, col=2, pch=3, lwd=3,cex=2)
    mtext(paste("notch D1 =", round(mean.notch1,2)), 3, line=2)
    mtext(paste("notch D2 =", round(mean.notch2,2)), 3, line=1)
    par(pty='m')
    plot(DF$notch1, 100*as.numeric(DF$passed)/as.numeric(DF$original), xlab="notch", ylab="Opp/Evt ratio (%)")
    points(DF$notch2, 100*as.numeric(DF$passed)/as.numeric(DF$original),col=2)
  #  plot(DF$notch1,type='o'); points(DF$notch2, col=2, type='o')
    plot(DF$notch1, DF$fsc.max, xlab="notch", ylab="forward scatter", ylim=c(1,65500))
    points(DF$notch2, DF$fsc.max, col=2)
    points(DF$notch1, DF$fsc.med, pch=3)
    points(DF$notch2, DF$fsc.med, pch=3, col=2)
    legend("center", legend=c("max FSC notch1", "max FSC notch2", "median FSC notch1","median FSC notch2"), pch=c(1,1,3,3), col=1:2, ncol=2)
  }

  return(DF)

}


#' Filter a list of EVT files.
#'
#' Filter a list of EVT files. Save OPP per file aggregate statistics to
#' SQLite3 database and save particle data to binary files in opp.dir.
#'
#' @param db SQLite3 database file path.
#' @param cruise.name Cruise name.
#' @param evt.dir EVT file directory.
#' @param evt.files List of EVT files to filter. Include julian day directory.
#' @param opp.dir OPP file output directory.
#' @param filter.id Optionally provide the ID for filter parameters. If NULL,
#'   the most recently saved filter parameters will be used.
#' @return None
#' @examples
#' \dontrun{
#' filter.evt.files(db, "testcruise", evt.dir, evt.files, opp.dir)
#' filter.evt.files(db, "testcruise", evt.dir, evt.files, opp.dir,
#'                  "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
filter.evt.files <- function(db, cruise.name, evt.dir, evt.files, opp.dir,
                             filter.id=NULL) {
  # Get notch and width to use from params file
  # Return empty data frame on warning or error
  if (is.null(filter.id)) {
    filter.params <- get.filter.params.latest(db)
  } else {
    filter.params <- get.filter.params.by.id(db, filter.id)
  }

  if (nrow(filter.params) == 0) {
    stop("No filter parameters defined")
  }

  i <- 0
  for (evt.file in evt.files) {
    message(round(100*i/length(evt.files)), "% completed \r", appendLF=FALSE)

    # Read EVT file
    # Return empty data frame on warning or error
    evt <- tryCatch({
      path <- file.path(evt.dir, evt.file)
      readSeaflow(path, transform=FALSE)
    }, warnings = function(err) {
      print(err)
      return(data.frame())
    }, error = function(err) {
      print(err)
      return(data.frame())
    })
    evt. <- evt[which(evt$fsc_small > 1 & evt$D1 > 1 & evt$D2 > 1),]
    evt_count = nrow(evt.)
    all_count = nrow(evt)

    # Filter EVT to OPP
    # Return empty data frame on warning or error
    result <- tryCatch({
      filter.evt(evt, filter.notch, origin=filter.params$origin,
                 width=filter.params$width, notch1=filter.params$notch1,
                 notch2=filter.params$notch2, offset=filter.params$offset)

    }, warnings = function(err) {
      print(err)
      return(list(opp=data.frame()))
    }, error = function(err) {
      print(err)
      return(list(opp=data.frame()))
    })

    # Upload OPP data
    delete.opp.stats.by.file(db, evt.file)
    delete.opp.by.file(opp.dir, evt.file)
    if (nrow(result$opp) > 0) {
      save.opp.stats(db, cruise.name, evt.file, all_count, evt_count, result$opp, result$params,
                     filter.params$id)
      save.opp.file(result$opp, opp.dir, evt.file)
    }

    i <-  i + 1
    flush.console()
  }
  message(round(100*i/length(evt.files)), "% completed \r", appendLF=FALSE)
  flush.console()
}

#' Filter a directory of EVT files using seaflowpy_filter
#'
#' Filter a list of EVT files. Save OPP per file aggregate statistics to
#' SQLite3 database and save particle data to binary files in opp.dir.
#'
#' @param db SQLite3 database file path.
#' @param cruise.name Cruise name.
#' @param evt.dir EVT file directory.
#' @param opp.dir OPP file output directory.
#' @param process.count Number of processes to start for filtering.
#' @param limit Only process up to this many files.
#' @param resolution Progress update resolution in \%.
#' @param origin,width,notch1,notch2,offset Filter parameters.
#' @return None
#' @examples
#' \dontrun{
#' seaflowpy_filter("testcruise.db", "testcruise", "./testcruise", "./testcruise_opp")
#' }
#' @export
seaflowpy_filter <- function(db, cruise.name, evt.dir, opp.dir, process.count=1, twopass=FALSE,
                             limit=NULL, resolution=NULL, origin=NULL,
                             width=NULL, notch1=NULL, notch2=NULL, offset=NULL) {

  # First check for seaflowpy_filter in PATH
  result <- tryCatch(
    {
      system2("bash", c("-lc", "'seaflowpy_filter --version'"), stdout=TRUE, stderr=TRUE)
    },
    warning=function(w) {
      invisible(w)
    },
    error=function(e) {
      return("system2error")
    }
  )
  if (result == "system2error") {
    warning("Could not run seaflowpy_filter")
    return()
  }

  cmd <- paste0("'seaflowpy_filter ", '-c "', cruise.name, '" -e "',
                normalizePath(evt.dir), '" -o "',  normalizePath(opp.dir),
                '" -d "', normalizePath(db), '"')
  if (! is.null(process.count)) {
    cmd <- paste0(cmd, " -p ", process.count)
  }
  if (! is.null(resolution)) {
    cmd <- paste0(cmd, " -r ", resolution)
  }
  if (twopass) {
    cmd <- paste0(cmd, " --twopass ")
  }
  ## {TO_DO} if twopass = TRUE, then notch1 and notch2 must be set to NULL, else ERROR
  if (! is.null(limit)) {
    cmd <- paste0(cmd, " -l ", limit)
  }
  if (! is.null(origin)) {
    cmd <- paste0(cmd, " --origin ", origin)
  }
  if (! is.null(width)) {
    cmd <- paste0(cmd, " --width ", width)
  }
  if (! is.null(notch1)) {
    cmd <- paste0(cmd, " --notch1 ", notch1)
  }
  if (! is.null(notch2)) {
    cmd <- paste0(cmd, " --notch2 ", notch2)
  }
  if (! is.null(offset)) {
    cmd <- paste0(cmd, " --offset ", offset)
  }
  cmd <- paste0(cmd, "'")
  system2("bash", c("-lc", cmd))
}
