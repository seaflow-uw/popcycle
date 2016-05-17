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
filter.notch <- function(evt, origin=NA, width=0.5, notch1=NA, notch2=NA, offset=0) {

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

#' find.filter.notch
#'
#' @export
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

    evt_count = nrow(evt)

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
      save.opp.stats(db, cruise.name, evt.file, evt_count, result$opp, result$params,
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
seaflowpy_filter <- function(db, cruise.name, evt.dir, opp.dir, process.count=1,
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
                evt.dir, '" -o "',  opp.dir, '" -d "', db, '"')
  if (! is.null(process.count)) {
    cmd <- paste0(cmd, " -p ", process.count)
  }
  if (! is.null(resolution)) {
    cmd <- paste0(cmd, " -r ", resolution)
  }
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
