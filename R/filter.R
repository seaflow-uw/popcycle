#' Filter EVT particles with a generic filter function.
#'
#' @param evt EVT data frame.
#' @param filter.func Filtering function.
#' @return Named list where list$params contains a list of filtering parameters
#'   and list$opp contains OPP data frame.
#' @examples
#' \dontrun{
#' filt <- filter.evt(evt, filter.notch, filter.params)
#' }
#' @usage filter.evt(evt, filter.func, ...)
#' @export filter.evt
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
#' @param filter.params Filtering parameters in a one row data frame or named
#'   list. Columns should include width, notch.small.D1, notch.small.D2,
#'   notch.large.D1, notch.large.D2, offset.small.D1, offset.small.D2,
#'   offset.large.D1, offset.large.D2.
#' @return Named list where list$params contains a list of filtering parameters
#'   and list$opp contains OPP data frame.
#' @examples
#' \dontrun{
#' filt <- filter.notch(evt, params)
#' }
#' @usage filter.notch(evt, filter.params)
#' @export filter.notch
filter.notch <- function(evt, filter.params) {
  width <- as.numeric(filter.params$width)
  notch.small.D1 <- as.numeric(filter.params$notch.small.D1)
  notch.small.D2 <- as.numeric(filter.params$notch.small.D2)
  notch.large.D1 <- as.numeric(filter.params$notch.large.D1)
  notch.large.D2 <- as.numeric(filter.params$notch.large.D2)
  offset.small.D1 <- as.numeric(filter.params$offset.small.D1)
  offset.small.D2 <- as.numeric(filter.params$offset.small.D2)
  offset.large.D1 <- as.numeric(filter.params$offset.large.D1)
  offset.large.D2 <- as.numeric(filter.params$offset.large.D2)

  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    return(list("opp"=data.frame(c())))
  }

  # linearize the LOG transformed data
  lin <- FALSE
  id <- which(colnames(evt) == "pulse_width" | colnames(evt) == "time" | colnames(evt) =="pop")
  if (!any(max(evt[,-c(id)]) > 10^3.5)) {
    evt <- untransformData(evt)
    lin <- TRUE
  }

  # Filtering out noise
  evt. <- evt[evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1, ]

  # Fltering aligned particles (D1 = D2)
  aligned <- subset(evt., D2 < D1 + width & D1 < D2 + width)

  # Filtering focused particles (fsc_small > D + notch)
  opp <- subset(aligned, D1 <= fsc_small*notch.small.D1 + offset.small.D1 & D2 <= fsc_small*notch.small.D2 + offset.small.D2 |
      D1  <= fsc_small*notch.large.D1 + offset.large.D1 & D2 <= fsc_small*notch.large.D2 + offset.large.D2)

      if(lin) opp <- transformData(opp)

  params = list("width"=width,
                "notch.small.D1"= notch.small.D1,"notch.small.D2"= notch.small.D2,
                "notch.large.D1"= notch.large.D1,"notch.large.D2"= notch.large.D2,
                "offset.small.D1"= offset.small.D1,"offset.small.D2"= offset.small.D2,
                "offset.large.D1"= offset.large.D1,"offset.large.D2"= offset.large.D2)
  return(list("opp"=opp, "params"=params))
}

#' Plot helpful cytograms for exploring filtering parameters.
#'
#' @param evt EVT data frame.
#' @param filter.params Filtering parameters in a one row data frame or named
#'   list. Columns should include width, notch.small.D1, notch.small.D2,
#'   notch.large.D1, notch.large.D2, offset.small.D1, offset.small.D2,
#'   offset.large.D1, offset.large.D2.
#' @return None
#' @usage plot.filter.cytogram(evt, filter.params)
#' @export plot.filter.cytogram
plot.filter.cytogram <- function(evt, filter.params) {
  width <- as.numeric(filter.params$width)
  notch.small.D1 <- as.numeric(filter.params$notch.small.D1)
  notch.small.D2 <- as.numeric(filter.params$notch.small.D2)
  notch.large.D1 <- as.numeric(filter.params$notch.large.D1)
  notch.large.D2 <- as.numeric(filter.params$notch.large.D2)
  offset.small.D1 <- as.numeric(filter.params$offset.small.D1)
  offset.small.D2 <- as.numeric(filter.params$offset.small.D2)
  offset.large.D1 <- as.numeric(filter.params$offset.large.D1)
  offset.large.D2 <- as.numeric(filter.params$offset.large.D2)

  # linearize the LOG transformed data
  id <- which(colnames(evt) == "fsc_small" | colnames(evt) == "chl_small" | colnames(evt) =="pe" | colnames(evt) =="fsc_perp" | colnames(evt) =="D1" | colnames(evt) =="D2")
  if (!any(max(evt[,c(id)]) > 10^3.5)) {
    evt[,c(id)] <- (log10(evt[,c(id)])/3.5)*2^16
  }

  # Filtering out noise
  evt. <- evt[evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1, ]

  # Fltering aligned particles (D1 = D2), with Correction for the difference of sensitivity between D1 and D2
  aligned <- subset(evt., D2 < D1 + width & D1 < D2 + width)

  # Filtering focused particles (fsc_small > D * notch)
  opp <- subset(aligned, D1 <= fsc_small*notch.small.D1 + offset.small.D1 & D2 <= fsc_small*notch.small.D2 + offset.small.D2 |
      D1  <= fsc_small*notch.large.D1 + offset.large.D1 & D2 <= fsc_small*notch.large.D2 + offset.large.D2)

  ################
  ### PLOTTING ###
  ################
  cols <- colorRampPalette(c("blue4","royalblue4","deepskyblue3", "seagreen3", "yellow", "orangered2","darkred"))
  percent.opp <- round(100*nrow(opp)/nrow(evt.),2)
  percent.noise <- round(100-100*nrow(evt.)/nrow(evt),0)

  if(nrow(evt.) > 10000)  evt. <- evt.[round(seq(1,nrow(evt.), length.out=10000)),]
  if(nrow(aligned) > 10000)  aligned <- aligned[round(seq(1,nrow(aligned), length.out=10000)),]

  def.par <- par(no.readonly = TRUE) # save default, for resetting...

  par(mfrow=c(2,3),pty='s')

  plot.cyt(evt., "D1", "D2")
  mtext("Alignment", side=3, line=3, font=2, col=2)
  abline(b=1, a=width, col='red',lwd=2)
  abline(b=1, a=-width, col='red',lwd=2)
  mtext(paste0("Noise = ", percent.noise, "%" ), side=3, line=1,font=2)

  plot.cyt(aligned, "fsc_small", "D1")
  mtext("Focus", side=3, line=3, font=2,col=2)
  abline(b=notch.small.D1, a=offset.small.D1,col=2)
  abline(b=notch.large.D1, a=offset.large.D1,col=3)
  points(filter.params$beads.fsc.small,filter.params$beads.D1, cex=2, pch=16)

  plot.cyt(aligned, "fsc_small", "D2")
  mtext("Focus", side=3, line=3, font=2,col=2)
  abline(b=notch.small.D2, a=offset.small.D2,col=2)
  abline(b=notch.large.D2, a=offset.large.D2,col=3)
  points(filter.params$beads.fsc.small,filter.params$beads.D2, cex=2, pch=16)

  plot.cyt(opp, "fsc_small", "pe"); abline(v=filter.params$beads.fsc.small, lty=2, col='grey')
  mtext("OPP", side=3, line=1, font=2, col=2)
  plot.cytogram(opp, "fsc_small","chl_small"); abline(v=filter.params$beads.fsc.small, lty=2, col='grey')
  mtext("OPP", side=3, line=1, font=2, col=2)
  plot.cytogram(opp, "chl_small","pe")
  mtext("OPP", side=3, line=1, font=2, col=2)
  mtext(paste("OPP =", percent.opp,"% EVT"), outer=T,side=1, line=-2,font=2,col=1)

  par(def.par)
}

#' Plot helpful cytograms for exploring filtering parameters for one file.
#'
#' @param evt.dir EVT file directory.
#' @param file.name File name with julian day directory.
#' @param filter.params Filtering parameters in a one row data frame or named
#'   list. Columns should include width, notch.small.D1, notch.small.D2,
#'   notch.large.D1, notch.large.D2, offset.small.D1, offset.small.D2,
#'   offset.large.D1, offset.large.D2.
#' @return None
#' @usage plot.filter.cytogram.by.file(evt.dir, file.name, filter.params)
#' @export plot.filter.cytogram.by.file
plot.filter.cytogram.by.file <- function(evt.dir, file.name, filter.params) {
  file.name <- clean.file.path(file.name)
  evt <- readSeaflow(file.path(evt.dir, file.name))
  plot.filter.cytogram(evt, filter.params)
}


#' Filter a list of EVT files.
#'
#' Filter a list of EVT files. Save OPP per file aggregate statistics to
#' SQLite3 database and save particle data to binary files in opp.dir.
#'
#' @param db SQLite3 database file path.
#' @param evt.dir EVT file directory.
#' @param evt.files List of EVT files to filter. Include julian day directory.
#' @param opp.dir OPP file output directory.
#' @param filter.id Optionally provide the ID for filter parameters. If NULL,
#'   the most recently saved filter parameters will be used.
#' @return None
#' @examples
#' \dontrun{
#' filter.evt.files(db, evt.dir, evt.files, opp.dir)
#' filter.evt.files(db, evt.dir, evt.files, opp.dir,
#'                  "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @usage filter.evt.files(db, evt.dir, evt.files, opp.dir, filter.id=NULL)
#' @export filter.evt.files
filter.evt.files <- function(db, evt.dir, evt.files, opp.dir,
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
    # Create empty data frame on warning or error
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

    if (nrow(evt) > 0) {
      # noise filter
      evt. <- evt[evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1, ]
      evt_count <- nrow(evt.)  # after noise filter
      all_count <- nrow(evt)   # before noise filter
    } else {
      evt_count <- 0
      all_count <- 0
    }

    # Delete all versions of this OPP file
    delete.opp.by.file(opp.dir, evt.file)
    delete.opp.stats.by.file(db, evt.file)

    quantile_opp_counts <- list(0, 0, 0)
    qi <- 1
    for (quantile in QUANTILES) {
      p <- filter.params[filter.params$quantile == quantile, ]
      # Filter EVT to OPP
      # Return empty data frame on warning or error
      result <- tryCatch({
        filter.evt(evt, filter.notch, p)
      }, warnings = function(err) {
        print(err)
        return(list(opp=data.frame()))
      }, error = function(err) {
        print(err)
        return(list(opp=data.frame()))
      })

      # Save OPP data
      err <- tryCatch({
        save.opp.stats(db, evt.file, all_count, evt_count,
                       result$opp, p$id, quantile)
      }, error = function(e) {
        cat(paste0("Error saving opp results to db with file ", evt.file, " for quantile ", quantile, ": ", e))
      })
      if (inherits(err, "error")) {
        delete.opp.by.file(opp.dir, evt.file)   # clean up opp files
        delete.opp.stats.by.file(db, evt.file)  # clean up any db entry
        break
      }
      err <- tryCatch({
        if (nrow(result$opp) > 0) {
          save.opp.file(result$opp, opp.dir, evt.file, quantile)
        }
      }, error = function(e) {
        cat(paste0("Error saving opp file ", evt.file, " for quantile ", quantile, ": ", e))
      })
      if (inherits(err, "error")) {
        delete.opp.by.file(opp.dir, evt.file)   # clean up opp files
        delete.opp.stats.by.file(db, evt.file)  # clean up any db entry
        break
      }
      # store how many focused particles there were for this quantile
      quantile_opp_counts[qi] <- nrow(result$opp)
      qi <- qi + 1
    }

    # If all quantiles didn't produce OPP for this EVT file, erase any OPP
    # files that were created, but keep all DB entries
    if (! all(quantile_opp_counts > 0)) {
      delete.opp.by.file(opp.dir, evt.file)
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
#' @param width,notch.small.D1, notch.small.D2, notch.large.D1, notch.large.D2, offset.small.D1, offset.small.D2, offset.large.D1, offset.large.D2 Filter parameters.
#' @return None
#' @examples
#' \dontrun{
#' seaflowpy_filter("testcruise.db", "testcruise", "./testcruise", "./testcruise_opp")
#' }
#' @export
seaflowpy_filter <- function(db, cruise.name, evt.dir, opp.dir, process.count=1, twopass=FALSE,
                             limit=NULL, resolution=NULL, width=NULL,
                             notch.small.D1=NULL, notch.small.D2=NULL,
                             notch.large.D1=NULL, notch.large.D2=NULL,
                             offset.small.D1=NULL, offset.small.D2=NULL,
                             offset.large.D1=NULL, offset.large.D2=NULL){

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
  if (! is.null(limit)) {
    cmd <- paste0(cmd, " -l ", limit)
  }
  if (! is.null(width)) {
    cmd <- paste0(cmd, " --width ", width)
  }
  if (! is.null(notch.small.D1)) {
    cmd <- paste0(cmd, " --notch.small.D1 ", notch.small.D1)
  }
  if (! is.null(notch.small.D2)) {
    cmd <- paste0(cmd, " --notch.small.D2 ", notch.small.D2)
  }
  if (! is.null(notch.largel.D1)) {
    cmd <- paste0(cmd, " --notch.large.D1 ", notch.large.D1)
  }
  if (! is.null(notch.large.D2)) {
    cmd <- paste0(cmd, " --notch.large.D2 ", notch.large.D2)
  }
  if (! is.null(offset.small.D1)) {
    cmd <- paste0(cmd, " --offset.small.D1 ", offset.small.D1)
  }
  if (! is.null(offset.small.D2)) {
    cmd <- paste0(cmd, " --offset.small.D2 ", offset.small.D2)
  }
  if (! is.null(offset.largel.D1)) {
    cmd <- paste0(cmd, " --offset.large.D1 ", offset.large.D1)
  }
  if (! is.null(offset.large.D2)) {
    cmd <- paste0(cmd, " --offset.large.D2 ", offset.large.D2)
  }
  cmd <- paste0(cmd, "'")
  system2("bash", c("-lc", cmd))
}
