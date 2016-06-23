#' Delete an entry in the opp table by file name.
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
  sql.dbGetQuery(db, sql)
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
  opp.file <- paste0(file.path(opp.dir, clean.file.path(file.name)), ".opp")
  if (file.exists(opp.file)) {
    file.remove(opp.file)
  }
  if (file.exists(paste0(opp.file, ".gz"))) {
    file.remove(paste0(opp.file, ".gz"))
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
  sql.dbGetQuery(db, sql)
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
  vct.file <- paste0(file.path(vct.dir, clean.file.path(file.name)), ".vct")
  if (file.exists(vct.file)) {
    file.remove(vct.file)
  }
  if (file.exists(paste0(vct.file, ".gz"))) {
    file.remove(paste0(vct.file, ".gz"))
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
  sql.dbGetQuery(db, sql)
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
  sql.dbGetQuery(db, sql)
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
  sql.dbGetQuery(db, sql)
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
reset.table <- function(db, table.name) {
  sql <- paste0("DELETE FROM ", table.name)
  sql.dbGetQuery(db, sql)
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
    sfl.cruise == opp.cruise
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
      sfl.cruise == opp.cruise
      AND
      sfl.file == opp.file"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=T)
  opp <- sql.dbGetQuery(db, sql)
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
    sfl.cruise == vct.cruise
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
      sfl.cruise == vct.cruise
      AND
      sfl.file == vct.file"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=T)
  vct <- sql.dbGetQuery(db, sql)
  return(vct)
}

#' Get OPP data frame by date.
#'
#' Requires a populated sfl table.
#'
#' @param db SQLite3 data file path.
#' @param opp.dir OPP file directory.
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
get.opp.by.date <- function(db, opp.dir, start.date, end.date, channel=NULL,
                            transform=TRUE, vct.dir=NULL, pop=NULL) {
  opp.stats <- get.opp.stats.by.date(db, start.date=start.date, end.date=end.date)
  opp <- get.opp.by.file(opp.dir, opp.stats$file, channel=channel,
                         transform=transform, vct.dir=vct.dir, pop=pop)
  return(opp)
}

#' Get OPP data frame by file.
#'
#' @param opp.dir OPP file directory.
#' @param file.name File name with julian day directory. Can be a single
#'   file name or vector of file names.
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
get.opp.by.file <- function(opp.dir, file.name, channel=NULL,
                            transform=TRUE, vct.dir=NULL, pop=NULL) {
  file.name.clean <- unlist(lapply(file.name, clean.file.path))
  opp.files <- paste0(file.name.clean, ".opp")
  opp.reader <- function(f) {
    path <- file.path(opp.dir, f)
    opp <- readSeaflow(path, channel=channel, transform=transform)
    return(opp)
  }
  opps <- lapply(opp.files, opp.reader)
  opps.bound <- rbind.fill(opps)

  if (! is.null(vct.dir)) {
   vcts <- lapply(file.name.clean, function(f) get.vct.by.file(vct.dir, f))
   vcts.bound <- rbind.fill(vcts)
   opps.bound <- cbind(opps.bound, vcts.bound)
   if (! is.null(pop)) {
     opps.bound <- opps.bound[opps.bound$pop == pop, ]
   }
  }
  return(opps.bound)
}

#' Get data frame of per particle population classifications by file'
#'
#' @param vct.dir VCT file directory.
#' @param file.name File name with julian day directory.
#' @return Data frame of per particle population classifications.
#' @examples
#' \dontrun{
#' vct <- get.vct.by.file(vct.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
get.vct.by.file <- function(vct.dir, file.name) {
  vct.file <- paste0(file.path(vct.dir, clean.file.path(file.name)), ".vct")
  if (!file.exists(vct.file)) {
    vct.file <- paste0(vct.file, ".gz")
    if (!file.exists(vct.file)) {
      stop(paste("No VCT file for ", file.name))
    }
  }
  if (file_ext(vct.file) == "gz") {
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
  sql <- paste0("SELECT * FROM filter WHERE id = '", filter.id, "'")
  result <- sql.dbGetQuery(db, sql)
  return(result)
}

#' Get the latest gating parameters.
#'
#' @param db SQLite3 database file path.
#' @return Named list where list$row is gating entry in gating table and
#'   list$poly.log is list of polygon coordinates for each population.
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
#' @return Named list where list$row is gating entry in gating table and
#'   list$poly.log is list of polygon coordinates for each population.
#' @examples
#' \dontrun{
#' gating.params <- get.gating.params.by.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
get.gating.params.by.id <- function(db, gating.id) {
  sql <- paste0("SELECT * FROM gating WHERE id = '", gating.id, "'")
  gating.df <- sql.dbGetQuery(db, sql)
  if (nrow(gating.df) == 0) {
    stop(paste0("No entry found in gating table for ", gating.id))
  }
  poly.log <- get.poly.log.by.gating.id(db, gating.id, gating.df$pop_order[1])
  answer <- list(row=gating.df, poly.log=poly.log)
  return(answer)
}

#' Construct a gating polygon named list for gating.id
#'
#' @param db SQLite3 database file path.
#' @param gating.id Foreign key to gating table.
#' @param pop.order Comma-separated text specifying population order. This is
#'   the pop_order field in the gating table.
#' @return List of population gating polygon coordinates.
#' @examples
#' \dontrun{
#' poly.log <- get.poly.log.by.gating.id(db, "d3afb1ea-ad20-46cf-866d-869300fe17f4",
#'                                       "beads,prochloro,synecho")
#' }
#' @export
get.poly.log.by.gating.id <- function(db, gating.id, pop.order) {
  poly.log <- list()
  pop.names <- strsplit(pop.order, ",")[[1]]
  for (i in seq(length(pop.names))) {
    sql <- paste0("
      SELECT * FROM poly
      WHERE
        gating_id = '", gating.id, "'
        AND
        pop = '", pop.names[i], "'
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
    poly.log[[i]] <- as.matrix(pop.poly)
  }
  names(poly.log) <- pop.names
  return(poly.log)
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
      sfl.cruise == opp.cruise
      AND
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
      sfl.cruise == vct.cruise
      AND
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
  sql <- "SELECT * FROM poly ORDER BY gating_id, point_order ASC"
  poly <- sql.dbGetQuery(db, sql)
  return(poly)
}

#' Get aggregate statistics data frame joining sfl, opp, and vct table entries.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of aggregate statistics.
#' @examples
#' \dontrun{
#' stat.table <- get.stat.table(db)
#' }
#' @export
get.stat.table <- function(db) {
  check.for.populated.sfl(db)
  sql <- "SELECT * FROM stat;"
  stats <- sql.dbGetQuery(db, sql)
  return(stats)
}

#' Get names for EVT files which produced no OPP data.
#'
#' @param db SQLite3 database file path.
#' @param evt.files list of EVT file paths, e.g. get.evt.files(evt.dir).
#' @return List of EVT files which produced no OPP data.
#' @examples
#' \dontrun{
#' empty.evt.files <- get.empty.evt.files(db, evt.files)
#' }
#' @export
get.empty.evt.files <- function(db, evt.files) {
  opp.files <- get.opp.files(db)
  evt.files <- unlist(lapply(evt.files, function(f) { clean.file.path(f) }))
  return(setdiff(evt.files, opp.files))
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


#' Get OPP file names.
#'
#' @param db SQLite3 database file path.
#' @return List of distinct OPP file names.
#' @examples
#' \dontrun{
#' opp.files <- get.opp.files(db)
#' }
#' @export
get.opp.files <- function(db) {
  sql <- "SELECT DISTINCT file from opp"
  files <- sql.dbGetQuery(db, sql)
  print(paste(length(files$file), "opp files found"))
  return(files$file)
}

#' Get VCT file names.
#'
#' @param db SQLite3 database file path.
#' @return List of distinct VCT file names.
#' @examples
#' \dontrun{
#' vct.files <- get.vct.files(db)
#' }
#' @export
get.vct.files <- function(db) {
  sql <- "SELECT DISTINCT file from vct"
  files <- sql.dbGetQuery(db, sql)
  return(files$file)
}

#' Save VCT aggregate population statistics for one file to vct table.
#'
#' @param db SQLite3 database file path.
#' @param cruise.name  Cruise name.
#' @param file.name File name with julian day directory.
#' @param opp OPP data frame with pop column.
#' @param method Gating method name.
#' @param gating.id ID for entry in gating table.
#' @return None
#' @examples
#' \dontrun{
#' save.vct.stats(db, "testcruise", "2014_185/2014-07-04T00-00-02+00-00",
#'                opp, "Manual gating", "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
save.vct.stats <- function(db, cruise.name, file.name, opp, method, gating.id) {
  df <- ddply(opp, .(pop), here(summarize),
              cruise=cruise.name, file=clean.file.path(file.name),
              count=length(pop), method=method,
              fsc_small=mean(fsc_small), fsc_perp=mean(fsc_perp),
              chl_small=mean(chl_small), pe=mean(pe), gating_id=gating.id)
  cols <- c("cruise", "file", "pop", "count", "method", "fsc_small",
            "fsc_perp", "pe", "chl_small", "gating_id")
  df.reorder <- df[cols]
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="vct", value=df.reorder, row.names=F, append=T)
  dbDisconnect(con)
}

#' Save VCT per particle population classification.
#'
#' File will be saved to vct.dir as a gzipped text file, one line per particle.
#'
#' @param vct List of per particle population classifications.
#' @param vct.dir Output directory for VCT files.
#' @param file.name File name with julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' save.vct.file(db, vct.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
save.vct.file <- function(vct, vct.dir, file.name) {
  vct.file <- paste0(file.path(vct.dir, clean.file.path(file.name)), ".vct.gz")
  dir.create(dirname(vct.file), showWarnings=F, recursive=T)
  con <- gzfile(vct.file, "w")
  write.table(vct, con, row.names=F, col.names=F, quote=F)
  close(con)
}

#' Save OPP aggregate statistics for one file to opp table.
#'
#' @param db SQLite3 database file path.
#' @param cruise.name  Cruise name.
#' @param file.name File name with julian day directory.
#' @param evt_count Number of particles in EVT file.
#' @param opp OPP data frame with pop column.
#' @param params Named list of filter parameters, including any that were
#'   auto-calculated for this file.
#' @param filter.id ID for entry in filter table.
#' @return None
#' @examples
#' \dontrun{
#' save.opp.stats(db, "testcruise", "2014_185/2014-07-04T00-00-02+00-00",
#'                40000, opp, filter.params, "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
save.opp.stats <- function(db, cruise.name, file.name, evt_count, opp, params,
                           filter.id) {
  if (nrow(opp) == 0) {
    return
  }
  opp <- transformData(opp)
  opp_count <- nrow(opp)
  df <- data.frame(cruise=cruise.name, file=clean.file.path(file.name),
                   opp_count=opp_count, evt_count=evt_count,
                   opp_evt_ratio=opp_count/evt_count,
                   notch1=params$notch1, notch2=params$notch2,
                   offset=params$offset, origin=params$origin,
                   width=params$width,
                   fsc_small_min=min(opp$fsc_small),
                   fsc_small_max=max(opp$fsc_small),
                   fsc_small_mean=mean(opp$fsc_small),
                   fsc_perp_min=min(opp$fsc_perp),
                   fsc_perp_max=max(opp$fsc_perp),
                   fsc_perp_mean=mean(opp$fsc_perp),
                   fsc_big_min=min(opp$fsc_big),
                   fsc_big_max=max(opp$fsc_big),
                   fsc_big_mean=mean(opp$fsc_big),
                   pe_min=min(opp$pe),
                   pe_max=max(opp$pe),
                   pe_mean=mean(opp$pe),
                   chl_small_min=min(opp$chl_small),
                   chl_small_max=max(opp$chl_small),
                   chl_small_mean=mean(opp$chl_small),
                   chl_big_min=min(opp$chl_big),
                   chl_big_max=max(opp$chl_big),
                   chl_big_mean=mean(opp$chl_big),
                   filter_id=filter.id
        )
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="opp", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

#' Save OPP as a gzipped LabView format binary file
#'
#' @param opp OPP data frame of filtered particles.
#' @param opp.dir Output directory. Julian day sub-directories will be
#'   automatically created.
#' @param file.name File name with julian day directory.
#' @param untransform Convert linear data to log.
#' @return None
#' @examples
#' \dontrun{
#' save.opp.file(opp, opp.dir, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
save.opp.file <- function(opp, opp.dir, file.name, untransform=FALSE) {
  opp.file <- paste0(file.path(opp.dir, clean.file.path(file.name)), ".opp.gz")
  dir.create(dirname(opp.file), showWarnings=F, recursive=T)
  writeSeaflow(opp, opp.file, untransform=untransform)
}

#' Save filter parameters to the filter table.
#'
#' @param db SQLite3 database file path.
#' @param params Named list of filter parameters. Defaults to
#'   list(width=0.5, notch1=NA, notch2=NA, offset=0, origin=NA).
#' @return None
#' @examples
#' \dontrun{
#' save.filter.params(db)
#' save.fitter.params(db, list(notch1=.75, notch2=.5, offset=100, origin=NA, width=1.0))
#' }
#' @export
save.filter.params <- function(db, params=NULL) {
  if (is.null(params)) {
    # Default filter parameters
    params <- list(notch1=NA, notch2=NA, offset=0, origin=NA, width=0.5)
  }
  filter.id <- UUIDgenerate()  # create primary ID for new entry
  date.stamp <- format(Sys.time(),format="%FT%H:%M:%OS6+0000", tz="GMT")
  df <- data.frame(id=filter.id, date=date.stamp, notch1=params$notch1,
                   notch2=params$notch2, offset=params$offset,
                   origin=params$origin, width=params$width)
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="filter", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

#' Save gating parameters.
#'
#' This creates an entry in the gating table and saves gating polygon
#' coordinates in the poly table.
#'
#' @param db SQLite3 database file path.
#' @param poly.log Named list of per population gating polygons.
#' @return None
#' @examples
#' \dontrun{
#' save.gating.params(db, poly.log)
#' }
#' @export
save.gating.params <- function(db, poly.log) {
  gating.id <- UUIDgenerate()  # create primary ID for new entry
  date.stamp <- format(Sys.time(),format="%FT%H:%M:%OS6+0000", tz="GMT")
  df <- data.frame(id=gating.id, date=date.stamp,
                   pop_order=paste(names(poly.log), collapse=","))

  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="gating", value=df, row.names=F, append=T)
  dbDisconnect(con)

  save.poly(db, poly.log, gating.id)
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
save.poly <- function(db, poly.log, gating.id) {
  ns <- names(poly.log)
  df <- data.frame()
  channels <- c("fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small",
                "chl_big")

  if (length(ns) == 0) {
    return()
  }
  for (i in seq(length(ns))) {
    p <- poly.log[[ns[i]]]  # coords matrix for one population
    # Fill in population name. This is the first field in the table and sets up
    # the data frame to have the correct number of rows.
    tmpdf <- data.frame(pop=rep(ns[i], nrow(p)))
    for (col in channels) {
      # placeholder NAs for each channel
      # doing this first ensures the channel order matches the poly table
      # definition
      tmpdf[, col] <- NA
    }
    for (col in colnames(p)) {
      tmpdf[, col] <- p[, col]  # fill in defined channel coords
    }
    df <- rbind(df, tmpdf)
  }
  df$point_order <- seq(nrow(df))  # order of polygon points and populations
  df$gating_id <- gating.id  # last field in table

  delete.poly.by.id(db, gating.id)
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="poly", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

#' Import SFL files to the database.
#'
#' SFL input may come from all SFL files found in the EVT directory or from a
#' single SFL file. This function calls seaflowpy_importsfl.
#'
#' @param db SQLite3 database file path.
#' @param cruise Cruise name.
#' @param evt.dir EVT file directory with SFL files.
#' @param sfl.file Single SFL file to import.
#' @param gga lat/lon input is in GGA format. Set to TRUE to convert to decimal
#'   degree.
#' @param west Some ships don't provide E/W designations for longitude. Set to
#'   TRUE if this is the case and all longitudes should be West (negative).
#' @return None
#' @examples
#' \dontrun{
#' save.sfl(db, "testcruise", evt.dir=evt.dir)
#' save.sfl(db, "testcruise", sfl.file=sfl.file)
#' }
#' @export
save.sfl <- function(db, cruise, evt.dir=NULL, sfl.file=NULL, gga=FALSE,
                     west=FALSE) {
  # First check for seaflowpy_importsfl in PATH
  result <- tryCatch(
    {
      system2("bash", c("-lc", "'seaflowpy_importsfl --version'"), stdout=TRUE, stderr=TRUE)
      },
    warning=function(w) {
      invisible(w)
    },
    error=function(e) {
      return("system2error")
    }
  )
  if (result == "system2error") {
   warning("Could not run seaflowpy_importsfl")
   return()
  }
  if (is.null(evt.dir) && is.null(sfl.file)) {
    stop("save.sfl requires one of evt.dir or sfl.file")
  }
  if ((! is.null(evt.dir)) && (! is.null(sfl.file))) {
    stop("save.sfl can only be passed one of evt.dir or sfl.file")
  }

  cmd <- paste0("'seaflowpy_importsfl", ' -c "', cruise, '" -d "', normalizePath(db), '"')
  if (! is.null(evt.dir)) {
    cmd <- paste0(cmd, " -e '", normalizePath(evt.dir), "'")
  } else {
    cmd <- paste0(cmd, " -s '", normalizePath(sfl.file), "'")
  }

  if (gga) {
    cmd <- paste0(cmd, " --gga")
  }
  if (west) {
    cmd <- paste0(cmd, " --west")
  }
  cmd <- paste0(cmd, "'")
  system2("bash", c("-lc", cmd))
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
check.for.populated.sfl <- function(db) {
  if (nrow(get.sfl.table(db)) == 0) {
    stop("SFL table is empty")
  }
}

#' Wrapper to run dbGetQuery and clean up connection on error.
#'
#' @param db SQLite3 database file path.
#' @param sql SQL query to run.
#' @return Data frame returned by dbGetQuery.
#' @examples
#' \dontrun{
#' sql.dbGetQuery(db, "SELECT * FROM some.table")
#' }
sql.dbGetQuery <- function(db, sql) {
  con <- dbConnect(SQLite(), dbname=db)
  tryCatch({
    resp <- dbGetQuery(con, sql)
    dbDisconnect(con)
    return(resp)
  }, error=function(e) {
    dbDisconnect(con)
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
POSIXct.to.db.date <- function(date.ct) {
  return(format(date.ct, "%Y-%m-%dT%H:%M:00"))
}
