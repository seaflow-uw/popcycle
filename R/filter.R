filter.evt <- function(evt, filter.func, ...) {
  filt <- filter.func(evt, ...)

  # SANITY CHECKS
  # need same columns for opp
  if (!all(names(evt) == names(filt$opp))) {
    stop('Filtering function produced OPP with different columns')
  }

  # filtered all particles out?
  if (dim(filt$opp)[1] < 1) {
    stop('Filtering dropped all particles.')
  }

  return (filt)
}

filter.notch <- function(evt, origin=NA, width=0.5, notch=c(NA, NA), offset=0) {

  origin <- as.numeric(origin)
  width <- as.numeric(width)
  notch1 <- as.numeric(notch[1])
  notch2 <- as.numeric(notch[2])
  offset <- as.numeric(offset)

  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    return(data.frame(c()))
  }

  # linearize the LOG transformed data
  lin <- FALSE
  id <- which(colnames(evt) == "pulse_width" | colnames(evt) == "time" | colnames(evt) =="pop")
  if (!any(max(evt[,-c(id)]) > 10^3.5)) {
    evt <- .untransformData(evt)
    lin <- TRUE
  }

  # Correction for the difference of sensitivity between D1 and D2
  if(is.na(origin))  origin <- median(evt$D2-evt$D1)


  # Filtering particles detected by fsc_small
  evt. <- subset(evt, fsc_small > 1)

  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < (D1+origin) + width * 10^4 & (D1+origin) < D2 + width * 10^4)

 # finding the notch
  if (is.na(notch1)) {
    d.min1 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D1"])
    fsc.max1 <- max(aligned[which(aligned$D1 == d.min1),"fsc_small"])
    notch1 <- fsc.max1 / (d.min1+ 10000)
  }

  if (is.na(notch2)) {
    d.min2 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D2"])
    fsc.max2 <- max(aligned[which(aligned$D2 == d.min2),"fsc_small"])
    notch2 <- fsc.max2 / (d.min2 + 10000)
  }

  # Filtering focused particles (fsc_small > D + notch)
  opp <- subset(aligned, fsc_small > D1*notch1 - offset*10^4 & fsc_small > D2*notch2 - offset*10^4)

  params = list("notch1"=notch1, "notch2"=notch2, "offset"=offset,
                "origin"=origin, "width"=width)
  return(list("opp"=opp, "params"=params))
}

find.filter.notch <- function(evt.list, origin=NA, width=0.5, notch=c(NA, NA), offset=0, do.plot=TRUE){

  origin <- as.numeric(origin)
  width <- as.numeric(width)
  notch1 <- as.numeric(notch[1])
  notch2 <- as.numeric(notch[2])
  offset <- as.numeric(offset)


DF <- NULL

   for(file in evt.list){
      evt <- readSeaflow(file, transform=F)

      if(nrow(evt) == 0){
        print(paste("no evt found in", file))
        next
        }

      print(paste("processing ",file))
     # Correction for the difference of sensitivity between D1 and D2
        if(is.na(origin)) origin <- median(evt$D2-evt$D1)

     # Filtering particles detected by fsc_small
        evt. <- subset(evt, fsc_small > 0)

      # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
        aligned <- subset(evt., D2 < (D1+origin) + width * 10^4 & (D1+origin) < D2 + width * 10^4)

     # finding the notch
        if(is.na(notch1)){
          d.min1 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D1"])
          fsc.max1 <- max(aligned[which(aligned$D1 == d.min1),"fsc_small"])
          notch.1 <- fsc.max1 / (d.min1+ 10000)
            }

        if(is.na(notch2)){
          d.min2 <- min(aligned[which(aligned$fsc_small == max(aligned$fsc_small)),"D2"])
          fsc.max2 <- max(aligned[which(aligned$D2 == d.min2),"fsc_small"])
          notch.2 <- fsc.max2 / (d.min2 + 10000)
            }

        opp <- subset(aligned, fsc_small > D1*notch.1 - offset*10^4 & fsc_small > D2*notch.2 - offset*10^4)

      if(nrow(opp) == 0){
        print(paste("no opp found in", file))
        next
        }

      para <- data.frame(cbind(file=file, notch1=as.numeric(round(notch.1,2)), notch2=as.numeric(round(notch.2,2)),
                    fsc.med=as.numeric(round(median(opp$fsc_small))), chl.med=as.numeric(round(median(opp$chl_small))),
                    original=as.numeric(nrow(evt)), passed=as.numeric(nrow(opp))),stringsAsFactors = FALSE)

       DF <- rbind(DF, para)
      }

     if(do.plot){
    def.par <- par(no.readonly = TRUE) # save default, for resetting...

      best.notch1 <- mean(as.numeric(DF$notch1), na.rm=T)
      best.notch2 <- mean(as.numeric(DF$notch2), na.rm=T)


    par(mfrow=c(2,1),oma=c(2,2,2,4), cex=1)
    par(pty='s')
    plot(DF[,c('notch1', 'notch2')], asp=1)
    points(x=best.notch1, y=best.notch2, col=2, pch=3, lwd=3,cex=2)
    mtext(paste("Optimal notch D1 =", round(best.notch1,2)), 3, line=2)
    mtext(paste("Optimal notch D2 =", round(best.notch2,2)), 3, line=1)
    par(pty='m')
    plot(DF$notch1, 100*as.numeric(DF$passed)/as.numeric(DF$original), xlab="notch1", ylab="Opp/Evt ratio (%)")

  }

  return(DF)
}

# Filter a list of EVT files, save OPP per file aggregate data to sqlite and
# filtered particle to binary file.
#
# Args:
#   db: sqlite3 db path
#   cruise: cruise name
#   evt.dir: directory of evt files listed in evt.list
#   evt.list: list of EVT file paths, e.g. get.evt.list(evt.location)
#   opp.dir: directory for opp output files
filter.evt.files <- function(db, cruise, evt.dir, evt.list, opp.dir) {
  # Get notch and width to use from params file
  # Return empty data frame on warning or error
  params <- get.filter.params.latest(db)

  if (nrow(params) == 0) {
    stop("No filter parameters defined")
  }

  i <- 0
  for (evt.file in unlist(lapply(evt.list, clean.file.path))) {
    message(round(100*i/length(evt.list)), "% completed \r", appendLF=FALSE)

    # Read EVT file
    # Return empty data frame on warning or error
    evt <- tryCatch({
      readSeaflow(evt.file, path=evt.dir, transform=FALSE)
    }, warnings = function(err) {
      return(data.frame())
    }, error = function(err) {
      return(data.frame())
    })

    evt_count = nrow(evt)

    # Filter EVT to OPP
    # Return empty data frame on warning or error
    filt <- tryCatch({
      filter.evt(evt, filter.notch, origin=params$origin, width=params$width,
                 notch=c(params$notch1, params$notch2), offset=params$offset)
    }, warnings = function(err) {
      return(list(opp=data.frame()))
    }, error = function(err) {
      return(list(opp=data.frame()))
    })

    # Upload OPP data
    delete.opp.stats.by.file(db, evt.file)
    delete.opp.by.file(opp.dir, evt.file)
    if (nrow(filt$opp) > 0) {
      save.opp.stats(db, cruise, evt.file, evt_count, filt$opp, filt$params,
                     params$uuid)
      save.opp.file(filt$opp, opp.dir, evt.file)
    }

    i <-  i + 1
    flush.console()
  }
  message(round(100*i/length(evt.list)), "% completed \r", appendLF=FALSE)
  flush.console()
}
