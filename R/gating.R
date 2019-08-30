#' Define polygons for population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param poly.log Named list of gating polygon definitions. If a definition for
#'   popname already exists it will be updated. If it doesn"t exist it will be
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
  plot_cyt(opp, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- splancs::getpoly(quiet=TRUE) # Draw Gate
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

#' Define polygons for manual population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param gates.log Per population named list of classification parameters. If
#'   a definition for popname already exists it will be updated. If it doesn"t
#'   exist it will be appended to the end to the list. If NULL, a new list will
#'   be created.
#' @return gates.log with a new polygon defintion for popname.
#' @examples
#' \dontrun{
#' gates.log <- add.manual.classification(opp, "beads", "fsc_small", "pe")
#' gates.log <- add.manual.classification(opp, "prochloro", "fsc_small", "chl_small",
#'                                        gates.log)
#' }
#' @export
add.manual.classification <- function(opp, popname, para.x, para.y, gates.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  par(mfrow=c(1,1))
  plot_cyt(opp, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- splancs::getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para.x, para.y)

  poly.l <- list(method="manual", poly=poly)

  if (is.null(gates.log)) {
    # Start a new gating entry
    gates.log <- list()

  }
  gates.log[[popname]] <- poly.l
  return(gates.log)
}

#' Define polygons for manual population gating.
#'
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param position position parameter to FlowDensity
#' @param gates gates parameter to FlowDensity
#' @param scale scale parameter to FlowDensity
#' @param min.pe Only consider partciesl with pe > min.pe
#' @param gates.log Per population named list of classification parameters. If
#'   a definition for popname already exists it will be updated. If it doesn"t
#'   exist it will be appended to the end to the list. If NULL, a new list will
#'   be created.
#' @return gates.log with a new FlowDensity func call parameter set for popname
#' @examples
#' \dontrun{
#' gates.log <- add.auto.classification("beads", "fsc_small", "pe",
#'                                      c(FALSE,TRUE), c(2.0,NA), 0.975)
#' gates.log <- add.auto.classification("prochloro", "fsc_small", "chl_small",
#'                                      c(FALSE,TRUE), c(2.0,NA), 0.975, gates.log)
#' }
#' @export
add.auto.classification <- function(popname, para.x, para.y, position, gates,
                                    scale, min.pe=NA, gates.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  # Named list of important function parameters for FlowDensity
  params <- list(
    method="auto",
    x=para.x,
    y=para.y,
    position=position,
    gates=gates,
    scale=scale,
    min.pe=min.pe
  )

  if (is.null(gates.log)) {
    # Start a new gating entry
    gates.log <- list()
  }
  gates.log[[popname]] <- params

  return(gates.log)
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
#' @param params Named list of gating parameters. Must contain a params$poly
#'   entry with polygon definitions.
#' @param popname Name of the population
#' @return List of per particle classifications.
#' @examples
#' \dontrun{
#' vct <- manual.classify(opp, gates.log, "beads")
#' }
#' @export
manual.classify <- function(opp, params, popname) {
  if (is.null(opp$pop)) {
    opp$pop <- "unknown"
  }

  if (is.null(params)) {
    stop(paste0("No gate parameters found for ", popname))
  }

  poly <- params$poly # Get gating polygon definition
  para <- colnames(poly)  # channels
  df <- opp[opp$pop=="unknown", para]

  if (nrow(df) > 0) {
    colnames(poly) <- colnames(df) <- c("x","y") # to stop stupid Warnings from splancs::inout()
    vct <- df[splancs::inout(df,poly=poly, bound=TRUE, quiet=TRUE), ] # subset particles based on Gate
    opp[row.names(vct), "pop"] <- popname
  }

  return(opp)
}


#' Classify particles based on semisupervized clustering method from flowDensity package.
#'
#' @param opp OPP data frame.
#' @param params Named list of gating parameters. Must contain a params$poly
#'   entry with polygon definitions.
#' @param popname Name of the population
#' @return List of per particle classifications.
#' @examples
#' \dontrun{
#' vct <- auto.classify(opp, params, popname)
#' }
#' @export
auto.classify <- function(opp, params, popname) {
  if (is.null(opp$pop)) {
    opp$pop <- "unknown"
  }

  if (is.null(params)) {
    stop(paste("No gate parameters found for", popname))
  }

  # Only keep selected unknow pop rows and remove pop column
  if (is.null(params$min.pe) | is.na(params$min.pe)) {
   row.selection = opp$pop == "unknown"
  } else {
   row.selection = opp$pop == "unknown" & opp[,paste(params$x)] > params$min.pe
  }

  x <- opp[row.selection, names(opp) != "pop"]
  # Sometimes there are no unknowns at this point so check for zero rows
  if (nrow(x) > 0) {
    fframe <- flowCore::flowFrame(as.matrix(log10(x)))
    #plotDens(f, channels=c(5,8))
    channels <- c(match(params$x, names(x)), match(params$y, names(x)))
    labeled <- flowDensity::flowDensity(obj=fframe,channels=channels,
                                        position=params$position,
                                        gates=params$gates, ellip.gate=TRUE,
                                        scale=params$scale)
    opp[row.names(x[labeled@index,]),"pop"] <- popname
  }

  return(opp)
}

#' Classify particles from an OPP dataframe.
#'
#' Classify particles from an OPP dataframe using a gating scheme provided by gates.log.
#'
#' @param opp SQLite3 database file path.
#' @param gates.log A gating scheme from the function "add.manual.classification()" or "add.auto.classification()"
#' @return List of per particle classifications
#' @examples
#' \dontrun{
#' opp <- classify.opp(opp, gates.log)
#' }
#' @export
classify.opp <- function(opp, gates.log) {
  for (popname in names(gates.log)) {
    params <- gates.log[[popname]]
    if (params$method == "manual") {
      opp <- manual.classify(opp, params, popname)
    } else if (params$method == "auto") {
      opp <- auto.classify(opp, params, popname)
    } else {
      stop(paste("unrecognized classification method in classify.opp", params$method))
    }
  }
  if (! is.null(opp$pop)) {
    opp$pop <- factor(opp$pop)
  }
  return(opp)
}


#' Classify particles for a list of OPP files.
#'
#' Classify a  list of OPP files. Save per file aggregate population statistics
#' to SQLite3 database and save particle population definitions to text files
#' in vct.dir.
#'
#' @param db SQLite3 database file path.
#' @param opp.dir OPP file directory.
#' @param opp.files List of OPP files to classify. Include julian day directory.
#' @param vct.dir VCT file output directory.
#' @param gating.id Optionally provide the ID for gating parameters. If NULL,
#'   the existing gating parameters will be used.
#' @param vct.table Optionally provide the VCT table. If NULL,
#'   the existing gating parameters will be used.
#' @return None
#' @examples
#' \dontrun{
#' classify.opp.files(db, opp.dir, opp.files, vct.dir)
#' classify.opp.files(db, opp.dir, opp.files, vct.dir,
#'                    "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
classify.opp.files <- function(db, opp.dir, opp.files, vct.dir,
                               gating.id=NULL, vct.table=NULL) {
  if (!is.null(gating.id) & !is.null(vct.table)) {
    stop("gating.id and vct.table are mutually exclusive parameters")
  }
  if (!is.null(gating.id)) {
    # Use provided gating.id for all files
    gating.params <- get.gating.params.by.id(db, gating.id)
    if (length(gating.params$gates.log) == 0) {
      stop("No gate paramters yet; no gating.")
    }
  }
  if (is.null(gating.id) & is.null(vct.table)) {
    # No guidance on what gating parameters to used has been supplied.
    # Default to using the latest gating parameters for all files
    gating.params <- get.gating.params.latest(db)
  }
  if (is.null(gating.id) & !is.null(vct.table)) {
    opp_gates <- get.opp.gates(db, opp.files, vct.table)
  }

  # Always assume the latest filter parameters are the ones used to generate
  # the current OPP data
  filter.params <- get.filter.params.latest(db)
  if (is.null(filter.params)) {
    stop("No DB filter entries")
  }
  # Take from first quantile row, should be the same for all quantiles
  filter.id <- filter.params$id[1]

  # Get instrument serial number
  inst <- get.inst(db)

  i <- 0
  errors <- list()
  for (opp.file in opp.files) {
    message(round(100*i/length(opp.files)), "% completed \r", appendLF=FALSE)

    # delete old vct entries if they exist so we keep file/particle distinct
    # There should only be one vct entry in the db for each population/file
    # combination.
    delete.vct.stats.by.file(db, opp.file)
    delete.vct.by.file(vct.dir, opp.file)

    # get gating parameters (only if gating.id is NULL)
    if (is.null(gating.id) & !is.null(vct.table)) {
      id.gating <- opp_gates[opp_gates$file == opp.file, "gating_id"]
      gating.params <- get.gating.params.by.id(db, id.gating)
    }

    tryCatch({
      for (quantile in QUANTILES) {
        #print(paste("Loading", opp.file))
        opp <- get.opp.by.file(opp.dir, opp.file, quantile)

        #print(paste("Classifying", opp.file))
        # First calculate diameter and carbon quota
        beads.fsc <- as.numeric(transformData(data.frame(fsc=filter.params[which(filter.params$quantile == quantile),"beads.fsc.small"])))
        opp <- size.carbon.conversion(opp, beads.fsc=beads.fsc, inst=inst)

        # Then gate
        opp <- classify.opp(opp, gating.params$gates.log)

        # store vct
        #print("Uploading labels to the database")
        save.vct.stats(db, opp.file, opp, gating.params$id,
                       filter.id, quantile)

        vct <- opp[ , !(names(opp) %in% EVT.HEADER)]
        save.vct.file(vct, vct.dir, opp.file, quantile)
      }
    }, error = function(e) {
      cat(paste0("Error with file ", opp.file, ": ", e))
    })

    i <- i + 1
    flush.console()
  }
}

#' Get per file gating IDs to use for regating based on previous gating.
#'
#' Based on the time ranges different gating parameters were applied during
#' a previous gating run (as captured by vct_table), return a DataFrame with
#' OPP file to gating ID matches.
#'
#' @param db SQLite3 database file path.
#' @param opp_files List of OPP files to classify. Include julian day directory.
#' @param vct_table VCT table from past gating.
#' @return DataFrame of "file", "opp_dates", "gating_id"
#' @examples
#' \dontrun{
#' get.opp.gates(db, opp_files, vct_table)
#' }
#' @export
get.opp.gates <- function(db, opp_files, vct_table, verbose=TRUE) {
  # Get run length encoding results for gating ids. We"re trying to find the
  # boundaries of different gating parameters throughout the cruise.
  rle_result <- rle(vct_table$gating_id)
  gating_start_idx <- rle_starts(rle_result)  # start of each gating section

  # Get the first dates for each gating section
  gating_start_dates <- lubridate::ymd_hms(vct_table[gating_start_idx, "date"])

  if (verbose) {
    print("Timestamps during cruise where new gating parameters will be applied")
    tmpdf <- data.frame(
      date=gating_start_dates,
      gating_id=vct_table[gating_start_idx, "gating_id"]
    )
    print(tmpdf)
  }

  # Get files and dates
  opp <- get.opp.dates(db, opp_files)
  intervals <- findInterval(
    as.numeric(lubridate::ymd_hms(opp$date)),
    as.numeric(gating_start_dates)
  )

  # For values which are before any interval, findInterval returns an index of
  # 0. Convert those to 1 here, meaning use the first gating parameters for
  # these files.
  intervals <- sapply(intervals, function(x) {
    if (x == 0) { return(1) } else { return(x) }
  })

  gating_ids <- rle_result$values
  opp_gating_ids <- gating_ids[intervals]
  df <- data.frame(file=opp$file, opp_dates=opp$date, gating_id=opp_gating_ids)
  return(df)
}
