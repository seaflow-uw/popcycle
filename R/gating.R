#' Classify particles with a generic gating function.
#'
#' @param opp OPP data frame.
#' @param classify.func Classifying function.
#' @return List of per particle classifications.
#' @examples
#' \dontrun{
#' vct <- classify.opp(opp, ManualGating, poly.log)
#' }
#' @export
classify.opp <- function(opp, classify.func, ...) {
  vct <- classify.func(opp, ...)

  # SANITY CHECKS
  # dropped particles
  if (!(dim(opp)[1] == length(vct))) {
    stop('Filtering function returned incorrect number of labels.')
  }

  # in case classify.func didn't return text
  vct <- as.character(vct)

  return (vct)
}

#' Define polygons for population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param poly.log Named list of gating polygon definitions. If a definition for
#'   popname already exists it will be updated. If it doesn't exist it will be
#'   appended to the end to the list. If poly.log is NULL a new list will be
#'   created.
#' @return Version of poly.log with a new polygon defintion for popname.
#' @examples
#' \dontrun{
#' poly.log <- set.gating.params(opp, "beads", "fsc_small", "pe")
#' poly.log <- set.gating.params(opp, "prochloro", "fsc_small", "chl_small",
#'                               poly.log)
#' }
#' @export
set.gating.params <- function(opp, popname, para.x, para.y, poly.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  par(mfrow=c(1,1))
  plot.gating.cytogram(opp, poly.log, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para.x, para.y)

  poly.l <- list(poly)
  names(poly.l) <- popname

  if (is.null(poly.log)) {
    # Start a new gating entry
    poly.log <- poly.l
  } else {
    # if gate parameters for the same population already exist, overwrite,
    # otherwise append gate parameters for new population
    poly.log[popname] <- poly.l
  }
  return(poly.log)
}

#' Import old popcycle gating logs.
#'
#' Convert a list of old popcycle CSV gating log files into a poly.log
#' data structure identical to that produced by set.gating.params().
#'
#' @param csv.files CSV log files created by previous versions of popcycle
#' @param poly.log Named list of gating polygon definitions. Existing population
#'   gates will be updated. otherwise new ones will be appended to the list. If
#'   poly.log is NULL a new list will be created.
#' @return poly.log gating polygon definitions.
#' @examples
#' \dontrun{
#' poly.log <- gating.csv.to.poly.log(gates.csv.files)
#' }
#' @export
gating.csv.to.poly.log <- function(csv.files, poly.log=NULL) {
  for (onefile in csv.files) {
    # Find pop name from file name
    parts <- strsplit(onefile, "_")[[1]]
    last <- parts[length(parts)]  # e.g. beads.csv
    popname <- substr(last, 1, nchar(last) - 4)

    csv.poly <- read.csv(onefile)
    csv.poly <- list(as.matrix(csv.poly))
    names(csv.poly) <-popname

    if (is.null(poly.log)) {
      # Start a new gating entry
      poly.log <- csv.poly
    } else {
      # if gate parameters for the same population already exist, overwrite,
      # otherwise append gate parameters for new population
      poly.log[popname] <- csv.poly
    }
  }
  return(poly.log)
}

#' Classify particles based on manually defined population gates.
#'
#' @param opp OPP data frame.
#' @param poly.log Named list of gating polygon definitions. Particles will be
#'   classified by population in the order they appear in poly.log.
#' @return List of per particle classifications.
#' @examples
#' \dontrun{
#' vct <- ManualGating(opp, poly.log)
#' }
#' @export
ManualGating <- function(opp, poly.log){
  opp$pop <- "unknown"

  if (length(poly.log) == 0) {
    stop("No gate parameters found!")
   }

	for (i in 1:length(poly.log)) {
		pop <- names(poly.log[i]) # name of the population
    # print(paste('Gating',pop))
		poly <- poly.log[i][[1]] # Get parameters of the gate for this population
		para <- colnames(poly)
		df <- subset(opp, pop=="unknown")[,para]

		colnames(poly) <- colnames(df) <- c("x","y") # to stop stupid Warnings from inout()
		vct <- subset(df, inout(df,poly=poly, bound=TRUE, quiet=TRUE)) # subset particles based on Gate
		opp[row.names(vct),"pop"] <- pop
	}

	return(opp$pop)
}

#' Classifiy particles for a list of OPP files.
#'
#' Classify a  list of OPP files. Save per file aggregate population statistics
#' to SQLite3 database and save particle population definitions to text files
#' in vct.dir.
#'
#' @param db SQLite3 database file path.
#' @param cruise.name Cruise name.
#' @param opp.dir OPP file directory.
#' @param opp.files List of OPP files to classify. Include julian day directory.
#' @param vct.dir VCT file output directory.
#' @param gating.id Optionally provide the ID for gating parameters. If NULL,
#'   the most recently saved gating parameters will be used.
#' @return None
#' @examples
#' \dontrun{
#' classify.opp.files(db, "testcruise", opp.dir, opp.files, vct.dir)
#' classify.opp.files(db, "testcruise", opp.dir, opp.files, vct.dir,
#'                    "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
classify.opp.files <- function(db, cruise.name, opp.dir, opp.files, vct.dir,
                               gating.id=NULL) {
  if (is.null(gating.id)) {
    gating.params <- get.gating.params.latest(db)
  } else {
    gating.params <- get.gating.params.by.id(db, gating.id)
  }

  if (length(gating.params$poly.log) == 0) {
    stop('No gate paramters yet; no gating.')
  }

  i <- 0
  for (opp.file in opp.files) {
    message(round(100*i/length(opp.files)), "% completed \r", appendLF=FALSE)

    tryCatch({
      #print(paste('Loading', opp.file))
      opp <- get.opp.by.file(opp.dir, opp.file,
                             channel=c("fsc_small", "fsc_perp", "pe", "chl_small"))
      #print(paste('Classifying', opp.file))
      vct <- classify.opp(opp, ManualGating, gating.params$poly.log)

      opp$pop <- factor(vct)
      # delete old vct entries if they exist so we keep cruise/file/particle distinct
      delete.vct.stats.by.file(db, opp.file)
      delete.vct.by.file(vct.dir, opp.file)
      # store vct
      #print('Uploading labels to the database')
      save.vct.stats(db, cruise.name, opp.file, opp, 'Manual Gating',
                     gating.params$row$id)
      save.vct.file(vct, vct.dir, opp.file)
    }, error = function(e) {
      print(paste("Encountered error with file", opp.file))
      print(e)
    })

    i <-  i + 1
    flush.console()
  }
}

#' Classifiy particles for a list of OPP files using seaflowpy_classify.
#'
#' Classify a  list of OPP files. Save per file aggregate population statistics
#' to SQLite3 database and save particle population definitions to text files
#' in vct.dir.
#'
#' @param db SQLite3 database file path.
#' @param cruise.name Cruise name.
#' @param opp.dir OPP file directory.
#' @param opp.files List of OPP files to classify. Include julian day directory.
#' @param vct.dir VCT file output directory.
#' @param gating.id Optionally provide the ID for gating parameters. If NULL,
#'   the most recently saved gating parameters will be used.
#' @param process.count Number of processes to start for filtering.
#' @param limit Only process up to this many files.
#' @param resolution Progress update resolution in \%.
#' @param start.file First of subset of files to classify (including julian day dir)
#' @param end.file Last of subset of files to classify (including julian day dir)
#' @return None
#' @examples
#' \dontrun{
#' seaflowpy_classify(db, "testcruise", opp.dir, vct.dir,
#'                    "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
seaflowpy_classify <- function(db, cruise.name, opp.dir, vct.dir, gating.id=NULL,
                               process.count=1, limit=NULL, resolution=NULL,
                               start.file=NULL, end.file=NULL) {
  # First check for seaflowpy_classify in PATH
  result <- tryCatch(
   {
     system2("bash", c("-lc", "'seaflowpy_classify --version'"), stdout=TRUE, stderr=TRUE)
   },
   warning=function(w) {
     invisible(w)
   },
   error=function(e) {
     return("system2error")
   }
  )
  if (result == "system2error") {
   warning("Could not run seaflowpy_classify")
   return()
  }

  if (is.null(gating.id)) {
    gating.params <- get.gating.params.latest(db)
    gating.id <- gating.params$row$id
  } else {
    gating.params <- get.gating.params.by.id(db, gating.id)
  }

  if (length(gating.params$poly.log) == 0) {
    stop('No gate paramters yet; no gating.')
  }

  cmd <- paste0("'seaflowpy_classify ", '-c "', cruise.name,
                '" -o "', normalizePath(opp.dir),
                '" -v "', suppressWarnings(normalizePath(vct.dir)),
                '" -d "', normalizePath(db),
                '" -g "', gating.id, '"')
  if (! is.null(process.count)) {
    cmd <- paste0(cmd, " -p ", process.count)
  }
  if (! is.null(resolution)) {
    cmd <- paste0(cmd, " -r ", resolution)
  }
  if (! is.null(limit)) {
    cmd <- paste0(cmd, " -l ", limit)
  }
  if (! is.null(start.file)) {
    cmd <- paste0(cmd, " -s ", start.file)
  }
  if (! is.null(end.file)) {
    cmd <- paste0(cmd, " -e ", end.file)
  }
  cmd <- paste0(cmd, "'")
  system2("bash", c("-lc", cmd))
}
