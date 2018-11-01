#' Delete entries in the opp table by file name.
#'
#' @param db SQLite3 database file path.
#' @param file.name File name with julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' delete.opp.stats.by.file(db, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
delete.opp.stats.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM opp WHERE file == '", clean.file.path(file.name), "'")
  sql.dbExecute(db, sql)
}

#' Delete an OPP binary file by file name.
#'
#' @param opp.dir OPP file directory.
#' @param file.name File name with julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' delete.opp.by.file(opp.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
delete.opp.by.file <- function(opp.dir, file.name) {
  for (quantile in QUANTILES) {
    opp.file <- paste0(file.path(opp.dir, quantile, clean.file.path(file.name)), ".opp")
    if (file.exists(opp.file)) {
      file.remove(opp.file)
    }
    if (file.exists(paste0(opp.file, ".gz"))) {
      file.remove(paste0(opp.file, ".gz"))
    }
  }
}

#' Delete an entry in the vct table by file name.
#'
#' @param db SQLite3 database file path.
#' @param file.name File name with julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' delete.vct.stats.by.file(db, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
delete.vct.stats.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM vct WHERE file == '", clean.file.path(file.name), "'")
  sql.dbExecute(db, sql)
}

#' Delete a VCT text file by file name.
#'
#' @param vct.dir VCT file directory.
#' @param file.name File name with julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' delete.vct.by.file(opp.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
delete.vct.by.file <- function(vct.dir, file.name) {
  for (quantile in QUANTILES) {
    vct.file <- paste0(file.path(vct.dir, quantile, clean.file.path(file.name)), ".vct")
    if (file.exists(vct.file)) {
      file.remove(vct.file)
    }
    if (file.exists(paste0(vct.file, ".gz"))) {
      file.remove(paste0(vct.file, ".gz"))
    }
  }
}

#' Delete DB filter parameters by ID.
#'
#' @param db SQLite3 database file path.
#' @param filter.id ID for filter entries.
#' @return None
#' @examples
#' \dontrun{
#' delete.filter.params.by.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
delete.filter.params.by.id <- function(db, filter.id) {
  sql <- paste0("DELETE FROM filter WHERE id == '", filter.id, "'")
  sql.dbExecute(db, sql)
}

#' Delete DB gating parameters by gating ID.
#'
#' Any gating polygon entries in the poly table will also be deleted.
#'
#' @param db SQLite3 database file path.
#' @param gating.id ID for gating and poly entries.
#' @return None
#' @examples
#' \dontrun{
#' delete.gating.params.by.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
delete.gating.params.by.id <- function(db, gating.id) {
  sql <- paste0("DELETE FROM gating WHERE id == '", gating.id, "'")
  sql.dbExecute(db, sql)
  delete.poly.by.id(db, gating.id)
}

#' Delete DB poly parameters by gating ID.
#'
#' Note: This is usually done via delete.gating.params.by.id.
#'
#' @param db SQLite3 database file path.
#' @param gating.id gating_id for poly entries.
#' @return None
#' @examples
#' \dontrun{
#' delete.poly.by.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
delete.poly.by.id <- function(db, gating.id) {
  sql <- paste0("DELETE FROM poly WHERE gating_id == '", gating.id, "'")
  sql.dbExecute(db, sql)
}

#' Delete DB poly parameters by gating ID and population
#'
#' Note: This is usually done via delete.gating.params.by.id.
#'
#' @param db SQLite3 database file path.
#' @param gating.id gating_id for poly entries.
#' @param popname Population name
#' @return None
#' @examples
#' \dontrun{
#' delete.poly.by.id.pop(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4", "beads")
#' }
#' @export
delete.poly.by.id.pop <- function(db, gating.id, popname) {
  sql <- paste0("DELETE FROM poly WHERE gating_id == '", gating.id, "' AND pop == '", popname, "'")
  sql.dbExecute(db, sql)
}

#' Delete all rows in opp table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.opp.stats.table(db)
#' }
#' @export
reset.opp.stats.table <- function(db) {
  reset.table(db, "opp")
}

#' Delete all rows in vct table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.vct.stats.table(db)
#' }
#' @export
reset.vct.stats.table <- function(db) {
  reset.table(db, "vct")
}


#' Delete all rows in sfl table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.sfl.table(db)
#' }
#' @export
reset.sfl.table <- function(db) {
  reset.table(db, "sfl")
}

#' Delete all rows in filter table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.filter.table(db)
#' }
#' @export
reset.filter.table <- function(db) {
  reset.table(db, "filter")
}

#' Delete all rows in gating table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.gating.table(db)
#' }
#' @export
reset.gating.table <- function(db) {
  reset.table(db, "gating")
}

#' Delete all rows in poly table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.poly.table(db)
#' }
#' @export
reset.poly.table <- function(db) {
  reset.table(db, "poly")
}

#' Delete all rows in an arbitrary SQLite3 DB table.
#'
#' @param db SQLite3 database file path.
#' @param table.name Table name.
#' @return None
#' @examples
#' \dontrun{
#' reset.table(db, "opp")
#' }
#' @export

reset.table <- function(db, table.name) {
  sql <- paste0("DELETE FROM ", table.name)
  sql.dbExecute(db, sql)
}

#' Delete all rows in Outlier table.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.outlier.table(db)
#' }
#' @export
reset.outlier.table <- function(db) {
  reset.table(db, "outlier")
}


#' Remove entries for all tables except in filter, gating, and poly tables.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.db.except.params(db)
#' }
#' @export
reset.db.except.params <- function(db) {
  reset.opp.stats.table(db)
  reset.vct.stats.table(db)
  reset.sfl.table(db)
  reset.outlier.table(db)
}

#' Remove entries for all tables.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' reset.db(db)
#' }
#' @export
reset.db <- function(db) {
  reset.db.except.params(db)
  reset.filter.table(db)
  reset.gating.table(db)
  reset.poly.table(db)
}

#' Get OPP aggregated filtered particle statistics by file.
#'
#' Requires a populated sfl table.
#'
#' @param db SQLite3 data file path.
#' @param file.name File name with julian day directory.
#' @return Data frame of OPP aggregate statistics for one file.
#' @examples
#' \dontrun{
#' opp.stats <- get.opp.stats.by.file(db, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.opp.stats.by.file <- function(db, file.name) {
  check.for.populated.sfl(db)
  sql <- paste0("SELECT
    sfl.date, opp.*
  FROM
    sfl, opp
  WHERE
    opp.file == '", clean.file.path(file.name), "'
    AND
    sfl.file == opp.file
  ORDER BY sfl.date ASC")
  opp <- sql.dbGetQuery(db, sql)
  return(opp)
}

#' Get OPP aggregated filtered particle statistics by date.
#'
#' Requires a populated sfl table.
#'
#' @param db SQLite3 data file path.
#' @param start.date Start date in format YYYY-MM-DD HH:MM.
#' @param end.date End date in format YYYY-MM-DD HH:MM.
#' @return Data frame of OPP aggregate statistics for all files between
#'   start.date and end.date inclusive.
#' @examples
#' \dontrun{
#' opp.stats <- get.opp.stats.by.date(db, "2014-07-04 00:00", "2014-07-04 00:10")
#' }
#' @export
get.opp.stats.by.date <- function(db, start.date, end.date) {
  check.for.populated.sfl(db)
  sql <- "
    SELECT
      sfl.date, opp.*
    FROM
      sfl, opp
    WHERE
      sfl.file == opp.file"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=T)
  opp <- sql.dbGetQuery(db, sql)
  opp.files <- get.opp.files(db)  # get opp files with particles in all quantiles
  opp <- opp[opp$file %in% opp.files, ]
  return(opp)
}

#' Get VCT aggregated per population statistics by file.
#'
#' Requires a populated sfl table.
#'
#' @param db SQLite3 data file path.
#' @param file.name File name with julian day directory.
#' @return Data frame of VCT aggregate statistics for one file.
#' @examples
#' \dontrun{
#' vct.stats <- get.vct.stats.by.file(db, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.vct.stats.by.file <- function(db, file.name) {
  check.for.populated.sfl(db)
  sql <- paste0("SELECT
    sfl.date, vct.*
  FROM
    sfl, vct
  WHERE
    vct.file == '", clean.file.path(file.name), "'
    AND
    sfl.file == vct.file
  ORDER BY sfl.date ASC")
  vct <- sql.dbGetQuery(db, sql)
  return(vct)
}

#' Get VCT aggregated per population statistics by date.
#'
#' Requires a populated sfl table.
#'
#' @param db SQLite3 data file path.
#' @param start.date Start date in format YYYY-MM-DD HH:MM.
#' @param end.date End date in format YYYY-MM-DD HH:MM.
#' @return Data frame of VCT aggregate statistics for all files between
#'   start.date and end.date inclusive.
#' @examples
#' \dontrun{
#' vct.stats <- get.vct.stats.by.date(db, "2014-07-04 00:00", "2014-07-04 00:10")
#' }
#' @export
get.vct.stats.by.date <- function(db, start.date, end.date) {
  check.for.populated.sfl(db)
  sql <- "
    SELECT
      sfl.date, vct.*
    FROM
      sfl, vct
    WHERE
      sfl.file == vct.file"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=T)
  vct <- sql.dbGetQuery(db, sql)
  return(vct)
}

#' Get OPP data frame by date and quantile
#'
#' Requires a populated sfl table.
#'
#' @param db SQLite3 data file path.
#' @param opp.dir OPP file directory.
#' @param quantile Filtering quantile for this file
#' @param start.date Start date in format YYYY-MM-DD HH:MM.
#' @param end.date End date in format YYYY-MM-DD HH:MM.
#' @param channel Channels to keep in returned data frame. Can be a single name
#'   or a vector. Choosing fewer channels can significantly speed up retrieval.
#' @param transform Linearize OPP data.
#' @param vct.dir VCT file directory. If not specified returned data frame will
#'   not have a pop column.
#' @param pop If specified, the returned data frame will only contain entries
#'   for this population.
#' @return Data frame of OPP data for all files between start.date and end.date
#'   inclusive.
#' @examples
#' \dontrun{
#' opp <- get.opp.by.date(db, opp.dir, "2014-07-04 00:00", "2014-07-04 00:10")
#' opp <- get.opp.by.date(db, opp.dir, "2014-07-04 00:00", "2014-07-04 00:10",
#'                        channel=c("fsc_small", "chl_small", "pe"),
#'                        transform=F, vct.dir=vct.dir)
#' }
#' @export
get.opp.by.date <- function(db, opp.dir, quantile, start.date, end.date,
                            channel=NULL, transform=TRUE, vct.dir=NULL,
                            pop=NULL) {
  if(!is.null(pop) & is.null(vct.dir)) print("no vct data found, returning all opp instead")
  opp.stats <- get.opp.stats.by.date(db, start.date=start.date, end.date=end.date)
  # Filter for stats for one quantile
  opp.stats <- opp.stats[opp.stats$quantile == quantile, ]
  opp <- get.opp.by.file(opp.dir, opp.stats$file, quantile, channel=channel,
                         transform=transform, vct.dir=vct.dir, pop=pop)
  return(opp)
}

#' Get OPP data frame by file and quantile.
#'
#' @param opp.dir OPP file directory.
#' @param file.name File name with julian day directory. Can be a single
#'   file name or vector of file names.
#' @param quantile Filtering quantile for this file
#' @param channel Channels to keep in returned data frame. Can be a single name
#'   or a vector. Choosing fewer channels can significantly speed up retrieval.
#' @param transform Linearize OPP data.
#' @param vct.dir VCT file directory. If not specified returned data frame will
#'   not have a pop column.
#' @param pop If specified, the returned data frame will only contain entries
#'   for this population.
#' @return Data frame of OPP data for all files in file.name. If vct.dir is
#'   specified the data frame will have a per particle population annotations
#'   in a pop column.
#' @examples
#' \dontrun{
#' opp <- get.opp.by.file(opp.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' opp <- get.opp.by.file(opp.dir, "2014_185/2014-07-04T00-00-02+00-00",
#'                        channel=c("fsc_small", "chl_small", "pe"),
#'                        transform=F, vct.dir=vct.dir)
#' }
#' @export
get.opp.by.file <- function(opp.dir, file.name, quantile, channel=NULL,
                            transform=TRUE, vct.dir=NULL, pop=NULL) {
  file.name.clean <- unlist(lapply(file.name, clean.file.path))
  opp.files <- paste0(file.name.clean, ".opp")
  opp.reader <- function(f) {
    path <- file.path(opp.dir, quantile, f)
    opp <- readSeaflow(path, channel=channel, transform=transform)
    return(opp)
  }
  opps <- lapply(opp.files, opp.reader)
  opps.bound <- dplyr::bind_rows(opps)

  if(!is.null(pop) & is.null(vct.dir)) print("no vct data found, returning all opp instead")

  if (! is.null(vct.dir)) {
   vcts <- lapply(file.name.clean, function(f) get.vct.by.file(vct.dir, f, quantile))
   vcts.bound <- dplyr::bind_rows(vcts)
   opps.bound <- cbind(opps.bound, vcts.bound)
   if (! is.null(pop)) {
     opps.bound <- opps.bound[opps.bound$pop == pop, ]
   }
  }
  return(opps.bound)
}

#' Get data frame of per particle population classifications by file and quantile
#'
#' @param vct.dir VCT file directory.
#' @param file.name File name with julian day directory.
#' @param quantile Filtering quantile for this file
#' @return Data frame of per particle population classifications.
#' @examples
#' \dontrun{
#' vct <- get.vct.by.file(vct.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.vct.by.file <- function(vct.dir, file.name, quantile) {
  vct.file <- paste0(file.path(vct.dir, quantile, clean.file.path(file.name)), ".vct")
  if (!file.exists(vct.file)) {
    vct.file <- paste0(vct.file, ".gz")
    if (!file.exists(vct.file)) {
      stop(paste("No VCT file for ", file.name))
    }
  }
  if (tools::file_ext(vct.file) == "gz") {
    con <- gzfile(description=vct.file)
  } else {
    con <- file(description=vct.file)
  }
  vct <- read.table(con, col.names=c("pop"))
  return(vct)
}

#' Get SFL rows >= start.date and <= end.date.
#'
#' @param db SQLite3 database file path.
#' @param start.date Start date in format YYYY-MM-DD HH:MM.
#' @param end.date End date in format YYYY-MM-DD HH:MM.
#' @return Data frame of sfl table entries between start.date and end.date
#'   inclusive.
#' @examples
#' \dontrun{
#' sfl <- get.sfl.by.date(db, "2014-07-04 00:00", "2014-07-04 00:10")
#' }
#' @export
get.sfl.by.date <- function(db, start.date, end.date) {
  sql <- "SELECT * FROM sfl"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=F)
  sfl <- sql.dbGetQuery(db, sql)
  return(sfl)
}

#' Get the latest filter parameters.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of latest filter parameters.
#' @examples
#' \dontrun{
#' filter.params <- get.filter.params.latest(db)
#' }
#' @export
get.filter.params.latest <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date DESC LIMIT 1"
  result <- sql.dbGetQuery(db, sql)
  if (nrow(result) > 0) {
    result <- get.filter.params.by.id(db, result[1, "id"])
  }
  return(result)
}

#' Get filter parameters by id.
#'
#' @param db SQLite3 database file path.
#' @param filter.id ID for entry in filter table.
#' @return Data frame of filter parameters matchign filter.id.
#' @examples
#' \dontrun{
#' filter.params <- get.filter.params.by.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
get.filter.params.by.id <- function(db, filter.id) {
  sql <- paste0("SELECT * FROM filter WHERE id = '", filter.id, "' ORDER BY quantile")
  result <- sql.dbGetQuery(db, sql)
  # DB column names have underscores due to sqlite column naming restrictions.
  # To get this dataframe to match filter parameter column names used
  # elsewhere in this code base and to match R variable naming style
  # conventions, switch "_" for "." in all column names.
  names(result) <- unlist(lapply(names(result), function(n) {
    return(gsub("_", ".", n, fixed=T))
  }))
  return(result)
}

#' Get the latest gating parameters.
#'
#' @param db SQLite3 database file path.
#' @return Named list where list$gates.log is a recreation of original gating
#'   created by add.manual.classification(), add.auto.classification(), and
#'   save.gating.params()
#' @examples
#' \dontrun{
#' gating.params <- get.gating.params.latest(db)
#' }
#' @export
get.gating.params.latest <- function(db) {
  sql <- "SELECT * FROM gating ORDER BY date DESC LIMIT 1"
  gating.df <- sql.dbGetQuery(db, sql)
  if (nrow(gating.df) == 0) {
    stop("No entry found in gating table")
  }
  answer <- get.gating.params.by.id(db, gating.df$id[1])
  return(answer)
}

#' Get gating parameters by ID.
#'
#' @param db SQLite3 database file path.
#' @param gating.id ID in gating table and poly table.
#' @return Named list where list$gates.log a recreation of original gating
#'   created by add.manual.classification(), add.auto.classification(), and
#'   save.gating.params()
#' @examples
#' \dontrun{
#' gating.params <- get.gating.params.by.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
get.gating.params.by.id <- function(db, gating.id) {
  sql <- paste0("SELECT * FROM gating WHERE id = '", gating.id, "' ORDER BY pop_order ASC")
  gating.df <- sql.dbGetQuery(db, sql)
  if (nrow(gating.df) == 0) {
    stop(paste0("No entry found in gating table for ", gating.id))
  }

  gates.log <- list()

  for (i in seq(nrow(gating.df))) {
    r <- gating.df[i, ]
    if (r$method == "manual") {
      poly.log <- get.poly.log.by.gating.id.pop(db, gating.id, r$pop)
      gates.log[[r$pop]] <- list(method=r$method, poly=poly.log)
    } else if (r$method == "auto") {
      gates.log[[r$pop]] <- list(
        method=r$method,
        x=r$channel1,
        y=r$channel2,
        position=c(r$position1 == 1, r$position2 == 1),  # coerce to boolean from 1,0 integer stored in db
        gates=c(r$gate1, r$gate2),
        scale=r$scale,
        min.pe=r$minpe
      )
    } else {
      stop(paste0("unrecognized classification method ", r$method));
    }
  }

  answer <- list(id=gating.df[1, "id"], gates.log=gates.log)
  return(answer)
}

#' Construct a gating polygon list for gating.id pop combo
#'
#' @param db SQLite3 database file path.
#' @param gating.id Foreign key to gating table.
#' @param popname Population name
#' @return List of population gating polygon coordinates.
#' @examples
#' \dontrun{
#' poly.log <- get.poly.log.by.gating.id.pop(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4", "beads")
#' }
#' @export
get.poly.log.by.gating.id.pop <- function(db, gating.id, popname) {
  poly.log <- list()
  sql <- paste0("
    SELECT * FROM poly
    WHERE
      gating_id = '", gating.id, "'
      AND
      pop = '", popname, "'
    ORDER BY point_order"
  )
  pop.poly <- sql.dbGetQuery(db, sql)

  for (c in EVT.HEADER[5:length(EVT.HEADER)]) {
    if (c %in% colnames(pop.poly)) {
      if (all(is.na(pop.poly[, c]))) {
        pop.poly[, c] <- NULL
      }
    }
  }
  pop.poly[, "pop"] <- NULL
  pop.poly[, "gating_id"] <- NULL
  pop.poly[, "point_order"] <- NULL
  poly.log <- as.matrix(pop.poly)

  return(poly.log)
}
#' Get cruise name
#'
#' @param db SQLite3 database file path.
#' @return Cruise name
#' @examples
#' \dontrun{
#' cruise <- get.cruise(db)
#' }
#' @export
get.cruise <- function(db) {
  meta <- get.meta.table(db)
  if (nrow(meta) == 0) {
    stop(paste0("No cruise name found, metadata table is empty"))
  }
  return(meta$cruise[1])
}

#' Get instrument serial number
#'
#' @param db SQLite3 database file path.
#' @return One serial number
#' @examples
#' \dontrun{
#' inst <- get.inst(db)
#' }
#' @export
get.inst <- function(db) {
  meta <- get.meta.table(db)
  if (nrow(meta) == 0) {
    stop(paste0("No instrument serial found, metadata table is empty"))
  }
  return(meta$inst[1])
}

#' Get instrument serial number and cruise name
#'
#' @param db SQLite3 database file path.
#' @return Data frame.
#' @examples
#' \dontrun{
#' meta <- get.meta.table(db)
#' }
#' @export
get.meta.table <- function(db) {
  sql <- "SELECT * FROM metadata;"
  meta <- sql.dbGetQuery(db, sql)
  return(meta)
}

#' Return a data frame for the sfl table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of sfl table.
#' @examples
#' \dontrun{
#' sfl.table <- get.sfl.table(db)
#' }
#' @export
get.sfl.table <- function(db) {
  # Don't check for populated SFL table here since it should be obvious
  # if it's populated by result. Also, this would lead to infinite recursion
  # since check.for.populated.sfl calls this function.
  sql <- "SELECT * FROM sfl ORDER BY date ASC"
  sfl <- sql.dbGetQuery(db, sql)
  return(sfl)
}

#' Return a data frame for the opp table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of opp table.
#' @examples
#' \dontrun{
#' opp.table <- get.opp.table(db)
#' }
#' @export
get.opp.table <- function(db) {
  check.for.populated.sfl(db)
  sql <- "
    SELECT
      sfl.date, opp.*
    FROM
      sfl, opp
    WHERE
      sfl.file == opp.file
    ORDER BY sfl.date ASC"
  opp <- sql.dbGetQuery(db, sql)
  return(opp)
}

#' Return a data frame for the vct table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of vct table.
#' @examples
#' \dontrun{
#' vct.table <- get.vct.table(db)
#' }
#' @export
get.vct.table <- function(db) {
  check.for.populated.sfl(db)
  sql <- "
    SELECT
      sfl.date, vct.*
    FROM
      sfl, vct
    WHERE
      sfl.file == vct.file
    ORDER BY sfl.date ASC"
  vct <- sql.dbGetQuery(db, sql)
  return(vct)
}

#' Return a data frame for the filter table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of filter table.
#' @examples
#' \dontrun{
#' filter.table <- get.filter.table(db)
#' }
#' @export
get.filter.table <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date ASC"
  result <- sql.dbGetQuery(db, sql)
  return(result)
}

#' Return a data frame for the gating table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of gating table.
#' @examples
#' \dontrun{
#' gating.table <- get.gating.table(db)
#' }
#' @export
get.gating.table <- function(db) {
  sql <- "SELECT * FROM gating ORDER BY date ASC"
  gating <- sql.dbGetQuery(db, sql)
  return(gating)
}

#' Return a data frame for the poly table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of poly table.
#' @examples
#' \dontrun{
#' poly.table <- get.poly.table(db)
#' }
#' @export
get.poly.table <- function(db) {
  sql <- "SELECT * FROM poly ORDER BY gating_id, pop, point_order ASC"
  poly <- sql.dbGetQuery(db, sql)
  return(poly)
}


#' Get list of outliers.
#'
#' @param db SQLite3 database file path.
#' @return Data frame.
#' @examples
#' \dontrun{
#' stat.table <- get.stat.table(db)
#' }
#' @export
get.outlier.table <- function(db) {
  sql <- "SELECT * FROM outlier;"
  outlier <- sql.dbGetQuery(db, sql)
  return(outlier)
}


#' Get aggregate statistics data frame joining sfl, opp, and vct table entries.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' stat.table <- get.raw.stat.table(db)
#' }
#' @export
get.raw.stat.table <- function(db) {
  check.for.populated.sfl(db)
  sql <- "SELECT * FROM stat;"
  stat <- sql.dbGetQuery(db, sql)

  return(stat)
}


#' Get a list of EVT files by date range.
#'
#' @param db SQLite3 database file path.
#' @param evt.dir EVT file directory.
#' @param start.date Start date in format YYYY-MM-DD HH:MM.
#' @param end.date End date in format YYYY-MM-DD HH:MM.
#' @return Vector of EVT files within date range.
#' @examples
#' \dontrun{
#' evt.files <- get.evt.files.by.date(db, evt.dir, "2014-07-04 00:00", "2014-07-04 00:10")
#' }
#' @export
get.evt.files.by.date <- function(db, evt.dir, start.date, end.date) {
  check.for.populated.sfl(db)
  file.list <- get.evt.files(evt.dir)
  file.list <- unlist(lapply(file.list, clean.file.path))
  sfl <- get.sfl.by.date(db, start.date, end.date)

  if (nrow(sfl) == 0) {
    return(c())
  }

  start.file = sfl$file[1]
  end.file = sfl$file[nrow(sfl)]

  if(!any(file.list == start.file)) {
    stop(paste("Could not find file", start.file))
  }

  if(!any(file.list == end.file)) {
    stop(paste("Could not find file", end.file))
  }

  start.index = which(file.list == start.file)
  end.index = which(file.list == end.file)

  return(file.list[start.index:end.index])
}


#' Get OPP file names for data with focused particles in all quantiles.
#'
#' @param db SQLite3 database file path.
#' @param all.files Return all OPP files that were considered for processing,
#'   even entries where the raw EVT was unreadable or no focused particles made
#'   it through filtering.
#' @return List of OPP file names based on the latest filtering
#'   parameters or NULL if no filtering has been done.
#' @examples
#' \dontrun{
#' opp.files <- get.opp.files(db)
#' }
#' @export
get.opp.files <- function(db, all.files=FALSE) {
  filter.id <- get.filter.params.latest(db)$id
  if (is.null(filter.id)) {
    return(NULL)
  }
  filter.id <- filter.id[1]
  if (all.files) {
    sql <- paste0("SELECT file from opp WHERE filter_id = '", filter.id, "' GROUP BY file")
  } else {
    # Only return files where all quantiles produced OPP data
    sql <- paste0("SELECT file FROM opp WHERE opp_count > 0 AND filter_id = '", filter.id, "' GROUP BY file HAVING COUNT(*) == 3")
  }
  files <- sql.dbGetQuery(db, sql)
  return(files$file)
}

#' Get VCT file names.
#'
#' @param db SQLite3 database file path.
#' @return List of distinct VCT file names based on the latest gating
#'   parameters or NULL if no gating has been done.
#' @examples
#' \dontrun{
#' vct.files <- get.vct.files(db)
#' }
#' @export
get.vct.files <- function(db) {
  gating.id <- get.gating.params.latest(db)$id
  if (is.null(gating.id)) {
    return(NULL)
  }
  sql <- paste0("SELECT DISTINCT file from vct WHERE gating_id = '", gating.id, "'")
  files <- sql.dbGetQuery(db, sql)
  print(paste(length(files$file), "vct files found"))
  return(files$file)
}

#' Save VCT aggregate population statistics for one file to vct table.
#'
#' @param db SQLite3 database file path.
#' @param file.name File name with julian day directory.
#' @param opp OPP data frame with pop column.
#' @param gating.id ID for entry in gating table.
#' @param filter.id ID for entry in filter table.
#' @param quantile Filtering quantile for this file
#' @return None
#' @examples
#' \dontrun{
#' save.vct.stats(db, "2014_185/2014-07-04T00-00-02+00-00",
#'                opp, "d3afb1ea-ad20-46cf-866d-869300fe17f4",
#'                "0f3c89d5-b6a9-4959-95a8-dd2af73f82b9", 97.5)
#' }
#' @importFrom dplyr %>%
#' @export
save.vct.stats <- function(db, file.name, opp, gating.id,
                           filter.id, quantile) {
  df <- opp %>%
    dplyr::group_by(pop) %>%
    dplyr::summarise(
      file=clean.file.path(file.name),
      count=dplyr::n(),
      min_fsc_small=length(which(fsc_small <= quantile(fsc_small,0.1))),
      min_chl_small=length(which(chl_small <= quantile(chl_small,0.1))),
      sd_fsc_small=round(sd(fsc_small),3),
      sd_chl_small=round(sd(chl_small),3),
      n_peaks_fsc_small=length(which(diff(sign(diff(hist(log10(fsc_small), breaks=10, plot=F)$counts)))==-2)),
      n_peaks_chl_small=length(which(diff(sign(diff(hist(log10(chl_small), breaks=10, plot=F)$counts)))==-2)),
      gating_id=gating.id,
      filter_id=filter.id,
      quantile=quantile
    )
  cols <- c(
    "file", "pop", "count",
    "min_fsc_small", "min_chl_small",
    "sd_fsc_small", "sd_chl_small",
    "n_peaks_fsc_small", "n_peaks_chl_small",
    "gating_id", "filter_id", "quantile"
  )
  df.reorder <- as.data.frame(df)[cols]
  sql.dbWriteTable(db, name="vct", value=df.reorder)
}

#' Save VCT per particle population classification.
#'
#' File will be saved to vct.dir as a gzipped text file, one line per particle.
#'
#' @param vct List of per particle population classifications.
#' @param vct.dir Output directory for VCT files.
#' @param file.name File name with julian day directory.
#' @param quantile Filtering quantile for this file
#' @return None
#' @examples
#' \dontrun{
#' save.vct.file(db, vct.dir, "2014_185/2014-07-04T00-00-02+00-00", 97.5)
#' }
#' @export
save.vct.file <- function(vct, vct.dir, file.name, quantile) {
  vct.file <- paste0(file.path(vct.dir, quantile, clean.file.path(file.name)), ".vct.gz")
  dir.create(dirname(vct.file), showWarnings=F, recursive=T)
  con <- gzfile(vct.file, "w")
  write.table(vct, con, row.names=F, col.names=F, quote=F)
  close(con)
}

#' Save OPP aggregate statistics for one file/quantile combo to opp table.
#'
#' @param db SQLite3 database file path.
#' @param file.name File name with julian day directory.
#' @param evt_count Number of particles in EVT file.
#' @param opp OPP data frame with pop column.
#' @param filter.id ID for entry in filter table.
#' @param quantile Filtering quantile for this file
#' @return None
#' @examples
#' \dontrun{
#' save.opp.stats(db, "2014_185/2014-07-04T00-00-02+00-00",
#'                40000, opp, filter.params,
#'                "d3afb1ea-ad20-46cf-866d-869300fe17f4", 97.5)
#' }
#' @export
save.opp.stats <- function(db, file.name, all_count,
                           evt_count, opp, filter.id, quantile) {
  opp <- transformData(opp)
  opp_count <- nrow(opp)
  if (evt_count == 0) {
    opp_evt_ratio <- 0.0
  } else {
    opp_evt_ratio <- opp_count / evt_count
  }
  df <- data.frame(file=clean.file.path(file.name),
                   all_count=all_count,
                   opp_count=opp_count,
                   evt_count=evt_count,
                   opp_evt_ratio=opp_evt_ratio,
                   filter_id=filter.id,
                   quantile=quantile)
  sql.dbWriteTable(db, name="opp", value=df)
}

#' Save OPP as a gzipped LabView format binary file
#'
#' @param opp OPP data frame of filtered particles.
#' @param opp.dir Output directory. Julian day sub-directories will be
#'   automatically created.
#' @param file.name File name with julian day directory.
#' @param quantile Filtering quantile for this file
#' @param untransform Convert linear data to log.
#' @return None
#' @examples
#' \dontrun{
#' save.opp.file(opp, opp.dir, "2014_185/2014-07-04T00-00-02+00-00", 97.5)
#' }
#' @export
save.opp.file <- function(opp, opp.dir, file.name, quantile, untransform=FALSE) {
  opp.file <- paste0(file.path(opp.dir, quantile, clean.file.path(file.name)), ".opp.gz")
  dir.create(dirname(opp.file), showWarnings=F, recursive=T)
  writeSeaflow(opp, opp.file, untransform=untransform)
}

#' Save Outliers in the database
#'
#' @param db SQLite3 database file path.
#' @param table.name Dataframe that contains the list of files flagged as outliers
#' @return None
#' @examples
#' \dontrun{
#' save.outliers(db,  table.name)
#' }
#' @export
save.outliers <- function(db, table.name) {
  df <- data.frame(file=table.name$file, flag=table.name$flag)
  sql.dbWriteTable(db, name="outlier", value=df)
}

#' Save filter parameters to the filter table.
#'
#' @param db SQLite3 database file path.
#' @param beads.fsc.small Small forward scatter of 1µm beads used to determine
#'   filter.params
#' @param beads.D1 D1 of 1µm beads used to determine filter.params
#' @param beads.D1 D2 of 1µm beads used to determine filter.params
#' @param filter.params Data frame of filtering parameters one row per
#'   quantile. Columns should include:
#'   quantile, beads.fsc.small, beads.D1, beads.D2, width,
#'   notch.small.D1, notch.small.D2, notch.large.D1, notch.large.D2,
#'   offset.small.D1, offset.small.D2, offset.large.D1, offset.large.D2.
#' @return Database filter ID string.
#' @examples
#' \dontrun{
#' filter.id <- save.fitter.params(db, 1000, 2000, 3000, filter.params)
#' }
#' @export
save.filter.params <- function(db, filter.params) {
  filter.id <- uuid::UUIDgenerate()  # create ID for new entries
  date.stamp <- RFC3339.now()
  df <- data.frame()
  for (quantile in filter.params$quantile) {
    p <- filter.params[filter.params$quantile == quantile, ]
    if (nrow(p) > 1) {
      stop("Duplicate quantile rows found in parameters passed to save.filter.params()")
    }
    df <- rbind(df, cbind(id=filter.id, date=date.stamp, quantile=quantile,
                          beads_fsc_small=p$beads.fsc.small,
                          beads_D1=p$beads.D1,
                          beads_D2=p$beads.D2,
                          width=p$width,
                          notch_small_D1=p$notch.small.D1,
                          notch_small_D2=p$notch.small.D2,
                          notch_large_D1=p$notch.large.D1,
                          notch_large_D2=p$notch.large.D2,
                          offset_small_D1=p$offset.small.D1,
                          offset_small_D2=p$offset.small.D2,
                          offset_large_D1=p$offset.large.D1,
                          offset_large_D2=p$offset.large.D2))
  }
  sql.dbWriteTable(db, name="filter", value=df)
  return(filter.id)
}

#' Save gating parameters.
#'
#' This creates a set per population entries in the gating table and saves any
#' manual gating polygon coordinates in the poly table.
#'
#' @param db SQLite3 database file path.
#' @param gates.log Named list of per population classification parameters.
#' @return Database gating ID string.
#' @examples
#' \dontrun{
#' save.gating.params(db, gates.log)
#' }
#' @export
save.gating.params <- function(db, gates.log) {
  gating.id <- uuid::UUIDgenerate()  # create primary ID for new entry
  date.stamp <- RFC3339.now()
  i <- 1  # track order population classification
  for (popname in names(gates.log)) {
    params <- gates.log[[popname]]
    if (params$method == "manual") {
      df <- data.frame(
        id=gating.id, date=date.stamp, pop_order=i, pop=popname,
        method=params$method,
        channel1=colnames(params$poly)[1],
        channel2=colnames(params$poly)[2],
        gate1=NA,
        gate2=NA,
        position1=NA,
        position2=NA,
        scale=NA,
        minpe=NA
      )
      sql.dbWriteTable(db, name="gating", value=df)
      save.poly(db, params$poly, popname, gating.id)
    } else if (params$method == "auto") {
      df <- data.frame(
        id=gating.id, date=date.stamp, pop_order=i, pop=popname,
        method=params$method,
        channel1=params$x,
        channel2=params$y,
        gate1=params$gates[1],
        gate2=params$gates[2],
        position1=params$position[1],
        position2=params$position[2],
        scale=params$scale,
        minpe=params$min.pe
      )
      sql.dbWriteTable(db, name="gating", value=df)
    } else {
      stop(paste0("unrecognized method ", params$method))
    }
    i <- i + 1
  }
  return(gating.id)
}

#' Save gating polygon coordinates in the poly table.
#'
#' These entries will be linked to an entry in the gating table by gating.id.
#'
#' @param db SQLite3 database file path.
#' @param poly.log Named list of per population gating polygons.
#' @param gating.id Foreign key into gating table.
#' @return None
#' @examples
#' \dontrun{
#' save.poly(db, poly.log, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
save.poly <- function(db, poly.log, popname, gating.id) {
  df <- data.frame()
  channels <- c("fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small",
                "chl_big")
  # Fill in population name. This is the first field in the table and sets up
  # the data frame to have the correct number of rows.
  df <- data.frame(pop=rep(popname, nrow(poly.log)))
  for (col in channels) {
    # placeholder NAs for each channel
    # doing this first ensures the channel order matches the poly table
    # definition
    df[, col] <- NA
  }
  for (col in colnames(poly.log)) {
    df[, col] <- poly.log[, col]  # fill in defined channel coords
  }
  df$point_order <- seq(nrow(df))  # order of polygon points for this pop
  df$gating_id <- gating.id  # last field in table

  delete.poly.by.id.pop(db, gating.id, popname)
  sql.dbWriteTable(db, name="poly", value=df)

}

#' Import SFL files to the database.
#'
#' This function calls seaflowpy sfl db.
#'
#' @param db SQLite3 database file path.
#' @param cruise Cruise name. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @param sfl.file Single SFL file to import.
#' @return None
#' @examples
#' \dontrun{
#' save.sfl(db, sfl.file=sfl.file)
#' }
#' @export
save.sfl <- function(db, sfl.file, cruise=NULL, inst=NULL) {
  # First check for seaflowpy_sfl in PATH
  result <- tryCatch(
    {
      system2("seaflowpy", c("version"), stdout=TRUE, stderr=TRUE)
    },
    warning=function(w) {
      invisible(w)
    },
    error=function(e) {
      return("system2error")
    }
  )
  if (result == "system2error") {
   warning("Could not run seaflowpy")
   return()
  }
  if (is.null(sfl.file)) {
    stop("save.sfl requires sfl.file")
  }

  args <- c("sfl", "db", "-f", "-d", normalizePath(db), "-i", normalizePath(sfl.file))
  if (! is.null(cruise)) {
    args[length(args)+1] <- "-c"
    args[length(args)+1] <- cruise
  }
  if (! is.null(inst)) {
    args[length(args)+1] <- "-s"
    args[length(args)+1] <- inst
  }

  system2("seaflowpy", args, stdout=TRUE, stderr=TRUE)
}

#' Create a new, empty sqlite3 popcycle database.
#'
#' If a database already exists missing tables and indexes will be added.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' make.popcycle.db(db)
#' }
#' @export
make.popcycle.db <- function(db) {
  sql.file <- system.file(file.path("sql", "popcycle.sql"), package="popcycle")
  cmd <- sprintf("sqlite3 %s < %s", db, sql.file)
  status <- system(cmd)
  if (status > 0) {
    stop(paste("Db creation command '", cmd, "' failed with exit status ", status))
  }
}

#' Add a WHERE clause with date conditionals to SQL statement.
#'
#' Return a copy of SQL string with a WHERE clause added to find sfl.date
#' entries between start.date and end.date inclusive. If append is FALSE, a new
#' WHERE clause is added. If append is TRUE, conditionals are added to the end
#' of any existing WHERE statements.
#'
#' @param db SQLite3 database file path.
#' @param start.date Start date in format YYYY-MM-DD HH:MM.
#' @param end.date End date in format YYYY-MM-DD HH:MM
#' @return Original SQL string with new WHERE clause statements.
#' @examples
#' \dontrun{
#' sql <- sfl_date_where_class(sql, "2014-07-04 00:00", "2014-07-04 00:10")
#' sql <- sfl_date_where_class(sql, "2014-07-04 00:00", "2014-07-04 00:10",
#'                             append=T)
#' }
#' @export
sfl_date_where_clause <- function(sql, start.date, end.date, append=F) {
  if (! is.null(start.date) || ! is.null(end.date)) {
    if (! append) {
      sql <- paste0(sql, "
        WHERE"
      )
    } else {
      sql <- paste0(sql, "
        AND"
      )
    }

    if (! is.null(start.date)) {
      start.date <- paste0("sfl.date >= '", date.to.db.date(start.date), "'")
    }
    if (! is.null(end.date)) {
      end.date <- paste0("sfl.date <= '", date.to.db.date(end.date), "'")
    }
    sql <- paste0(sql, "
      ", paste(c(start.date, end.date), collapse=" AND ")
    )
  }

  sql <- paste0(sql, "
    ORDER BY sfl.date ASC"
  )
  return(sql);
}

#' Check if SFL table is populated.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @examples
#' \dontrun{
#' check.for.populated.sfl(db)
#' }
#' @export
check.for.populated.sfl <- function(db) {
  if (nrow(get.sfl.table(db)) == 0) {
    stop("SFL table is empty")
  }
}

#' Wrapper to run dbGetQuery and clean up connection on error.
#'
#' Use for SELECT statements only, otherwise use sql.dbExecute.
#'
#' @param db SQLite3 database file path.
#' @param sql SQL query to run.
#' @return Data frame returned by dbGetQuery.
#' @examples
#' \dontrun{
#' sql.dbGetQuery(db, "SELECT * FROM some.table")
#' }
#' @export
sql.dbGetQuery <- function(db, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db)
  tryCatch({
    resp <- DBI::dbGetQuery(con, sql)
    DBI::dbDisconnect(con)
    return(resp)
  }, error=function(e) {
    DBI::dbDisconnect(con)
    stop(e)
  })
}

#' Wrapper to run dbExecute and clean up connection on error.
#'
#' Use for any statement except SELECT, in which case use sql.dbGetQuery.
#'
#' @param db SQLite3 database file path.
#' @param sql SQL statement to run.
#' @return Number of rows affected
#' @examples
#' \dontrun{
#' sql.dbExecute(db, "DELETE FROM vct WHERE file == 'somefile'")
#' }
#' @export
sql.dbExecute <- function(db, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db)
  tryCatch({
    rows <- DBI::dbExecute(con, sql)
    DBI::dbDisconnect(con)
    return(rows)
  }, error=function(e) {
    DBI::dbDisconnect(con)
    stop(e)
  })
}

#' Wrapper to run dbWriteTable and clean up connection on error.
#'
#' @param db SQLite3 database file path.
#' @param name Table name.
#' @param value Data frame to write.
#' @examples
#' \dontrun{
#' sql.dbWriteTable(db, name="vct", value=df)
#' }
#' @export
sql.dbWriteTable <- function(db, name, value) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db)
  tryCatch({
    DBI::dbWriteTable(conn=con, name=name, value=value, row.names=F, append=T)
    DBI::dbDisconnect(con)
  }, error=function(e) {
    DBI::dbDisconnect(con)
    stop(e)
  })
}

#' Convert a date string to format suitable for popcycle db date comparison.
#'
#' @param date.string Date text in format YYYY-MM-DD HH:MM.
#' @return Date text as YYYY-MM-DDTHH:MM:00.
#' @examples
#' \dontrun{
#' db.date <- date.to.db.date("2014-01-24 13:03")
#' }
#' @export
date.to.db.date <- function(date.string) {
  return(POSIXct.to.db.date(string.to.POSIXct(date.string)))
}

#' Returns a POSIXct object for a human readable date string.
#'
#' @param date.string Date in format YYYY-MM-DD HH:MM.
#' @return POSIXct object.
#' @examples
#' \dontrun{
#' date.ct <- string.to.POSIXct("2014-01-24 13:03")
#' }
#' @export
string.to.POSIXct <- function(date.string) {
  # Make POSIXct objects in GMT time zone
  date.ct <- as.POSIXct(strptime(date.string, format="%Y-%m-%d %H:%M", tz="GMT"))
  if (is.na(date.ct)) {
    stop(paste("wrong format for date.string parameter : ", date.string,
               "instead of ", "%Y-%m-%d %H:%M"))
  }
  return(date.ct)
}

#' Convert a POSIXct date objectformat suitable for popcycle db date comparison.
#'
#' @param date.ct POSIXct date object.
#' @return Date text as YYYY-MM-DDTHH:MM:00.
#' @examples
#' \dontrun{
#' db.date <- POSIXct.to.db.date(date.ct)
#' }
#' @export
POSIXct.to.db.date <- function(date.ct) {
  return(format(date.ct, "%Y-%m-%dT%H:%M:00"))
}

#' Get current UTC datetime as RFC3339 string suitable for entry into db
#'
#' @return Date text as YYYY-MM-DDTHH:MM:SS+00:00."
#' @examples
#' \dontrun{
#' datetime.str <- RFC3339.now()
#' }
#' @export
RFC3339.now <- function() {
  # Now in local time POSIXct
  now.local <- Sys.time()
  # Change timezone to UTC
  attr(now.local, "tzone") <- "UTC"
  return(format(now.local, format="%FT%H:%M:%S+00:00"))
}

#' Find common database files between two directories.
#'
#' Directories will be search recursively and files will be matched by
#' basename. An error will be thrown if the same database file occurs more than
#  once in the same top-level directory.
#'
#' @param dir_a, dir_b Directories to compare.
#' @return Data Frame with columns for basename, old_path, and new_path
#' @examples
#' \dontrun{
#' common <- find_common_dbs(dir_a, dir_b)
#' }
find_common_dbs <- function(dir_a, dir_b) {
  # First find DB files with the same basename
  dbs_a <- list.files(dir_a, recursive=TRUE, pattern=".*\\.db")
  dbs_b <- list.files(dir_b, recursive=TRUE, pattern=".*\\.db")
  dbs_a_base <- sapply(dbs_a, basename)
  dbs_b_base <- sapply(dbs_b, basename)

  # stop if duplicated DBs, by basename, within either list
  dups_a <- duplicated(dbs_a_base)
  dups_b <- duplicated(dbs_b_base)
  if (any(dups_a)) {
    stop(paste0("Duplicated databases detected in ", dir_a, ": ", unique(dbs_a_base[dups_a])))
  }
  if (any(dups_b)) {
    stop(paste0("Duplicated databases detected in ", dir_b, ": ", unique(dbs_b[dups_b])))
  }

  # Find dbs in common
  common <- intersect(dbs_a_base, dbs_b_base)
  common_a_idx <- match(common, dbs_a_base)
  common_b_idx <- match(common, dbs_b_base)

  # Return db file matches as a dataframe with columns for basename,
  # old file paths, new file paths
  df <- data.frame(
    basename=common,
    old_path=sapply(dbs_a[common_a_idx], function(x) file.path(dir_a, x)),
    new_path=sapply(dbs_b[common_b_idx], function(x) file.path(dir_b, x)),
    stringsAsFactors=FALSE
  )
  row.names(df) <- NULL  # otherwise row names will equal names(common)
  return(df)
}

#' Copy tables from one popcycle database to another.
#'
#' The source database will remained unchanged. The destination database will
#' have selected table cleared before the copy. Schemas for source and
#' destination tables must match or an error is thrown.
#' @param db_from Popcycle database to copy tables from.
#' @param db_to Popcycle database to copy tables to.
#' @return None
#' @examples
#' \dontrun{
#' copy_tables(db_from, db_to)
#' }
copy_tables <- function(db_from, db_to, tables) {
  for (table_name in tables) {
    # Make sure schemas match for table to copy
    schema_query <- paste0("select * from sqlite_master where tbl_name = '", table_name, "'")
    schema_from <- sql.dbGetQuery(db_from, schema_query)
    schema_to <- sql.dbGetQuery(db_to, schema_query)
    if (! all.equal(schema_from, schema_to)) {
      stop(paste0("db files have differing schemas for ", table_name, " table"))
    }

    # Clear the db_to table
    reset.table(db_to, table_name)

    # Get the db_from table
    table_from <- sql.dbGetQuery(db_from, paste0("select * from ", table_name))

    # Save to db_to table
    sql.dbWriteTable(db_to, name=table_name, value=table_from)
  }
}
