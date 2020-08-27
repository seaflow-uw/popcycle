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
    stop("Filtering function produced OPP with different columns")
  }

  return (filt)
}

#' Filter EVT particles.
#'
#' @param evt EVT data frame.
#' @param filter.params Filtering parameters in data frame. Should contain
#'   parameters for all quantiles. Columns should include quantile, width,
#'   notch.small.D1, notch.small.D2, notch.large.D1, notch.large.D2,
#'   offset.small.D1, offset.small.D2, offset.large.D1, offset.large.D2.
#' @return OPP data frame with new logical columns for each quantile.
#' @examples
#' \dontrun{
#' filt <- filter.notch(evt, params)
#' }
#' @usage filter.notch(evt, filter.params)
#' @export filter.notch
filter.notch <- function(evt, filter.params) {
  # Check for empty evt data frame.  If empty return empty opp data frame.
  if (nrow(evt) == 0) {
    return(data.frame(c()))
  }

  # linearize the LOG transformed data
  lin <- FALSE
  columns <- (names(df) %in% c("D1", "D2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big"))
  if (!any(max(evt[, columns]) > 10^3.5)) {
    evt <- untransformData(evt)
    lin <- TRUE
  }
  # Assume width the same for all quantiles
  if (length(unique(filter.params[, "width"])) != 1) {
    stop("More than one width in filter parameters")
  }
  width <- as.numeric(filter.params[1, "width"])

  # Filtering out noise
  prefilter <- evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1
  # Filtering out particles with saturated  D1 or D2 signal
  # Here this means both D1 AND D2 must be below the max for either channel
  prefilter <- prefilter & evt$D1 < max(evt$D1) & evt$D2 < max(evt$D2)
  # Filtering aligned particles (D1 = D2)
  aligned_selector <- prefilter & (evt$D2 < evt$D1 + width) & (evt$D1 < evt$D2 + width)
  # Track which particles are OPP in any quantile
  opp_selector <- FALSE

  for (quantile in QUANTILES) {
    p <- filter.params[filter.params$quantile == quantile, ]
    notch.small.D1 <- as.numeric(p$notch.small.D1)
    notch.small.D2 <- as.numeric(p$notch.small.D2)
    notch.large.D1 <- as.numeric(p$notch.large.D1)
    notch.large.D2 <- as.numeric(p$notch.large.D2)
    offset.small.D1 <- as.numeric(p$offset.small.D1)
    offset.small.D2 <- as.numeric(p$offset.small.D2)
    offset.large.D1 <- as.numeric(p$offset.large.D1)
    offset.large.D2 <- as.numeric(p$offset.large.D2)

    # Filtering focused particles (fsc_small > D + notch)
    qopp_selector <- (((evt$D1 <= evt$fsc_small * notch.small.D1 + offset.small.D1) &
      (evt$D2 <= evt$fsc_small * notch.small.D2 + offset.small.D2)) |
      ((evt$D1 <= evt$fsc_small * notch.large.D1 + offset.large.D1) &
      (evt$D2 <= evt$fsc_small * notch.large.D2 + offset.large.D2))) &
      aligned_selector

    # Mark focused particles for this quantile in the original EVT dataframe
    qcolumn <- paste0("q", quantile)
    evt[qcolumn] = qopp_selector

    # Build a logical row selector which finds particles which are focused in
    # any quantile.
    opp_selector <- opp_selector | qopp_selector
  }

  opp <- evt[opp_selector, ]  # select for focused particles

  if (lin) {
    opp <- transformData(opp)
  }

  return(opp)
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

  # If SFL data is present, only filter EVT files in SFL table
  sfl.files <- get.sfl.table(db)$file
  if (length(sfl.files) > 0) {
    evt.files <- select_files_in(evt.files, sfl.files)
  }

  i <- 0
  for (evt.file in evt.files) {
    message(round(100*i/length(evt.files)), "% completed \r", appendLF=FALSE)

    # Read EVT file
    # Create empty data frame on warning or error
    evt <- tryCatch({
      get.evt.by.file(evt.dir, evt.file, transform=F)
    }, warnings = function(err) {
      print(err)
      return(data.frame())
    }, error = function(err) {
      print(err)
      return(data.frame())
    })

    if (nrow(evt) > 0) {
      # noise filter
      signal_selector <- (evt$fsc_small > 1 | evt$D1 > 1 | evt$D2 > 1)
      evt_count <- sum(signal_selector)  # after noise filter
      all_count <- nrow(evt)   # before noise filter
    } else {
      evt_count <- 0
      all_count <- 0
    }

    # Delete all versions of this OPP file
    delete.opp.by.file(opp.dir, evt.file)
    delete.opp.stats.by.file(db, evt.file)

    # Filter EVT to OPP
    # Return empty data frame on warning or error
    opp <- tryCatch({
      filter.evt(evt, filter.notch, filter.params)
    }, warnings = function(err) {
      print(err)
      return(data.frame())
    }, error = function(err) {
      print(err)
      return(data.frame())
    })

    # Save OPP data to database, regardless of whether this quantile has
    # any focused particles.
    err <- tryCatch({
      save.opp.stats(db, evt.file, all_count, evt_count, opp, unique(filter.params$id))
      save.outliers(db, data.frame(file=evt.file, flag=FLAG_OK))
    }, error = function(e) {
      cat(paste0("Error saving opp results to db with file ", evt.file, ": ", e))
    })
    if (inherits(err, "error")) {
      delete.opp.by.file(opp.dir, evt.file)   # clean up opp files
      delete.opp.stats.by.file(db, evt.file)  # clean up any db entry
      delete.outliers(db, evt.file)
      break
    }

    err <- tryCatch({
      if (nrow(opp) > 0) {
        save.opp.file(opp, opp.dir, evt.file)
      }
    }, error = function(e) {
      cat(paste0("Error saving opp file ", evt.file, ": ", e))
    })
    if (inherits(err, "error")) {
      delete.opp.by.file(opp.dir, evt.file)   # clean up opp files
      delete.opp.stats.by.file(db, evt.file)  # clean up any db entry
      break
    }

    i <-  i + 1
    flush.console()
  }
  message(round(100*i/length(evt.files)), "% completed \r", appendLF=FALSE)
  flush.console()
}

encode_bit_flags <- function(opp) {
  bit_flags <- NULL
  to_remove <- c()
  for (quantile in QUANTILES) {
    qcolumn <- paste0("q", quantile)
    shift_size <- log(QFLAGS[[qcolumn]], 2)
    if (is.null(bit_flags)) {
      bit_flags <- bitwShiftL(as.numeric(opp[, qcolumn]), shift_size)
    } else {
      bit_flags <- bitwOr(bit_flags, bitwShiftL(as.numeric(opp[, qcolumn]), shift_size))
    }
    to_remove <- c(to_remove, qcolumn)
  }
  opp["bitflags"] <- bit_flags
  opp <- dplyr::select(opp, -to_remove)
  return(opp)
}

decode_bit_flags <- function(opp) {
  bit_flags <- opp$bitflags
  opp <- dplyr::select(opp, -bitflags)
  for (quantile in QUANTILES) {
    qcolumn <- paste0("q", quantile)
    opp[qcolumn] <- (bitwAnd(bit_flags, as.integer(QFLAGS[qcolumn])) > 0)
  }
  return(opp)
}
