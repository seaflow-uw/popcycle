#' Plot helpful cytograms for estimating the D1, D2 and FSC coordinates of the inflection point (corresponds to location of 1µm beads).
#'
#' @param dataframe containing EVT data.
#' @return D1, D2 and fsc values of presumed 1 µm beads
#' @export
inflection.point <- function(DF){

  def.par <- par(no.readonly = TRUE) # save default, for resetting...
  par(mfrow=c(1,3),pty='s')

  plot.cytogram(DF, "fsc_small", "pe")
    poly.beads <- getpoly(quiet=TRUE)
    b <- subset(DF,inout(DF[,c("fsc_small", "pe")],poly=poly.beads, bound=TRUE, quiet=TRUE))

  plot.cytogram(DF, "fsc_small", "D1");points(b$fsc_small, b$D1,col=2, pch = 16, cex = 0.5)
      polyd1 <- getpoly(quiet=TRUE)

  plot.cytogram(DF, "fsc_small", "D2");points(b$fsc_small, b$D2,col=2,pch = 16, cex = 0.5)
      polyd2 <- getpoly(quiet=TRUE)

      fsc <- round(mean(c(polyd1[,1], polyd2[,1])))
      d1 <- round(mean(polyd1[,2]))
      d2 <- round(mean(polyd2[,2]))
      inflection <- data.frame(fsc, d1, d2)

  par(def.par)

  return(inflection)
}

#' Get Notch and Offset values for Filter.notch function.
#'
#' @param inst Instrument serial number
#' @param fsc Small forward scatter value of 1 µm beads
#' @param d1 D1 value of 1 µm beads
#' @param d2 D2 value of 1 µm beads
#' @param slopes User-supplied filter slope CSV file, overriding the installed
#'   default file.
#' @return Data frame with filtering parameters for 2.5, 50.0, 97.5 quantiles
#' @examples
#' \dontrun{
#' filt <- create.filter.params(inst, fsc, d1, d2)
#' }
#' @export
create.filter.params <- function(inst, fsc, d1, d2, slope.file=NULL) {
  # Rename to get correct dataframe headers
  beads.fsc.small <- as.numeric(fsc)
  beads.D1 <- as.numeric(d1)
  beads.D2 <- as.numeric(d2)

  width <- 2500

  if (is.null(slope.file)) {
    slope.file <- system.file("filter", "seaflow_filter_slopes.csv",package='popcycle')
  }
  slopes <- read.csv(slope.file)

  filter.params <- data.frame()
  headers <- c("quantile", "beads.fsc.small",
               "beads.D1", "beads.D2", "width",
               "notch.small.D1", "notch.small.D2",
               "notch.large.D1", "notch.large.D2",
               "offset.small.D1", "offset.small.D2",
               "offset.large.D1", "offset.large.D2")

  for (quantile in QUANTILES) {
    if (quantile == 2.5) {
      suffix <- "_2.5"
    } else if (quantile == 97.5) {
      suffix <- "_97.5"
    } else if (quantile == 50.0) {
      suffix <- ""
    }

    # Small particles
    notch.small.D1 <- beads.D1/beads.fsc.small  * slopes[slopes$ins== inst, paste0('notch.small.D1', suffix)] / slopes[slopes$ins== inst, 'notch.small.D1']
    notch.small.D2 <- beads.D2/beads.fsc.small  * slopes[slopes$ins== inst, paste0('notch.small.D2', suffix)] / slopes[slopes$ins== inst, 'notch.small.D2']
    offset.small.D1 <- 0
    offset.small.D2 <- 0
  
    # Large particles
    notch.large.D1 <- slopes[slopes$ins== inst, paste0('notch.large.D1', suffix)]
    notch.large.D2 <- slopes[slopes$ins== inst, paste0('notch.large.D2', suffix)]
    offset.large.D1 <- round(beads.D1 - notch.large.D1 * beads.fsc.small)
    offset.large.D2 <- round(beads.D2 - notch.large.D2 * beads.fsc.small)

      newrow <- data.frame(quantile, beads.fsc.small,
                         beads.D1, beads.D2, width,
                         notch.small.D1, notch.small.D2,
                         notch.large.D1, notch.large.D2,
                         offset.small.D1, offset.small.D2,
                         offset.large.D1, offset.large.D2,
                         stringsAsFactors=FALSE)
    names(newrow) <- headers
    filter.params <- rbind(filter.params, newrow)
  }

  return(filter.params)
}

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
#' @export
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
#' @export
plot.filter.cytogram <- function(evt, filter.params) {
  width <- 2500
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

  plot.cytogram(evt., "D1", "D2")
  mtext("Alignment", side=3, line=3, font=2, col=2)
  abline(b=1, a=width, col='red',lwd=2)
  abline(b=1, a=- width, col='red',lwd=2)
  plotrix:::draw.circle(0,0, radius=2000, border=2, lwd=2)
  mtext(paste0("Noise = ", percent.noise, "%" ), side=3, line=2, font=2)
  mtext(paste("Width=", width),side=3, line=1,font=2)

  plot.cytogram(aligned, "fsc_small", "D1")
  mtext("Focus", side=3, line=3, font=2,col=2)
  abline(b=notch.small.D1, a=offset.small.D1,col=2)
  abline(b=notch.large.D1, a=offset.large.D1,col=3)

  plot.cytogram(aligned, "fsc_small", "D2")
  mtext("Focus", side=3, line=3, font=2,col=2)
  abline(b=notch.small.D2, a=offset.small.D2,col=2)
  abline(b=notch.large.D2, a=offset.large.D2,col=3)

  plot.cytogram(opp, "fsc_small", "pe")
  mtext("OPP", side=3, line=1, font=2, col=2)
  plot.cytogram(opp, "fsc_small","chl_small")
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
#' @export
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
#' @export
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
    evt. <- evt[evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1, ]
    evt_count <- nrow(evt.)
    all_count <- nrow(evt)

    # Delete all versions of this OPP file
    delete.opp.by.file(opp.dir, evt.file)

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
      if (nrow(result$opp) > 0) {
        save.opp.stats(db, evt.file, all_count, evt_count,
                       result$opp, p$id, quantile)
        save.opp.file(result$opp, opp.dir, evt.file, quantile)
      }
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
