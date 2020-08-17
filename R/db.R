
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

#' Delete entries in the outlier table by file name.
#'
#' @param db SQLite3 database file path.
#' @param file.name File name with julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' delete.outliers.by.file(db, "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
delete.outliers.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM outlier WHERE file == '", clean.file.path(file.name), "'")
  sql.dbExecute(db, sql)
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
    sfl
  INNER JOIN opp ON sfl.file == opp.file
  WHERE
    opp.file == '", clean.file.path(file.name), "'
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
#' @param outliers If TRUE, remove data flaged as outliers
#' @return Data frame of OPP aggregate statistics for all files between
#'   start.date and end.date inclusive.
#' @examples
#' \dontrun{
#' opp.stats <- get.opp.stats.by.date(db, "2014-07-04 00:00", "2014-07-04 00:10")
#' }
#' @export
get.opp.stats.by.date <- function(db, start.date, end.date, outliers=TRUE) {
  check.for.populated.sfl(db)
  sql <- paste0(
    "SELECT sfl.date, opp.*\n",
    "FROM sfl\n",
    "INNER JOIN opp ON sfl.file == opp.file\n",
    opp_quantile_inner_join_clause(),
    "INNER JOIN outlier ON sfl.file == outlier.file\n",
    "WHERE\n",
    sfl_date_where_clause(start.date, end.date)
  )
  if (outliers) {
    sql <- paste0(
      sql,
      "AND\n",
      "outlier.flag == ", FLAG_OK, "\n"
    )
  }
  sql <- paste0(sql,  "ORDER BY sfl.date ASC")
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
  sql <- paste0("
    SELECT sfl.date, vct.*
    FROM sfl
    INNER JOIN vct on sfl.file == vct.file
    WHERE vct.file == '", clean.file.path(file.name), "'
    ORDER BY sfl.date ASC
  ")
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
#' @param outliers If TRUE, remove data flaged as outliers
#' @return Data frame of VCT aggregate statistics for all files between
#'   start.date and end.date inclusive.
#' @examples
#' \dontrun{
#' vct.stats <- get.vct.stats.by.date(db, "2014-07-04 00:00", "2014-07-04 00:10")
#' }
#' @export
get.vct.stats.by.date <- function(db, start.date, end.date, outliers=TRUE) {
  check.for.populated.sfl(db)
  sql <- paste0(
    "SELECT sfl.date, vct.*\n",
    "FROM sfl\n",
    "INNER JOIN vct on sfl.file == vct.file\n",
    opp_quantile_inner_join_clause(),
    "INNER JOIN outlier ON sfl.file == outlier.file\n",
    "WHERE\n",
    sfl_date_where_clause(start.date, end.date)
  )
  if (outliers) {
    sql <- paste0(
      sql,
      "AND\n",
      "outlier.flag == ", FLAG_OK, "\n"
    )
  }
  sql <- paste0(sql,  "ORDER BY sfl.date ASC")
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
#' @param outliers If TRUE, remove data flaged as outliers
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
                            pop=NULL, outliers=TRUE) {
  if(!is.null(pop) & is.null(vct.dir)) print("no vct data found, returning all opp instead")
  opp.stats <- get.opp.stats.by.date(db, start.date=start.date, end.date=end.date, outliers=outliers)
  # Filter for stats for one quantile
  opp.stats <- opp.stats[opp.stats$quantile == quantile, ]

  #retrieve data
  opp <- get.opp.by.file(opp.dir, opp.stats$file, quantile=quantile, channel=channel,
                         transform=transform, vct.dir=vct.dir, pop=pop)
  return(opp)
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
  sql <- paste0(
    "SELECT * FROM sfl\n",
    "WHERE\n",
    sfl_date_where_clause(start.date, end.date)
  )
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
    FROM opp
    INNER JOIN sfl ON sfl.file == opp.file
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
    FROM sfl
    INNER JOIN vct ON sfl.file == vct.file
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
#' outlier.table <- get.outlier.table(db)
#' }
#' @export
get.outlier.table <- function(db) {
  sql <- "SELECT * FROM outlier ORDER BY file;"
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

#' Get POSIXct date from SFL table for file IDs.
#'
#' @param db SQLite3 database file path.
#' @param file_ids Character vector of file IDs.
#' @return Tibble with columns "date" and "file_id", with same order as file_ids.
#' @examples
#' \dontrun{
#' df <- get_file_dates(db, file_ids)
#' }
#' @export
get_file_dates <- function(db, file_ids) {
  sfl <- tibble::as_tibble(get.sfl.table(db))
  sfl <- dplyr::select(sfl, date, file_id=file)
  sfl$date <- lubridate::ymd_hms(sfl$date)
  files <- tibble::tibble(file_id=file_ids)
  result <- dplyr::left_join(files, sfl, by="file_id")
  return(result[, c("date", "file_id")])
}

#' Get ISO8601 datetime strings (e.g. 2018-07-13T22:24:40+00:00) for OPP files.
#'
#' @param db SQLite3 database file path.
#' @param opp_files Character vector of OPP file IDs.
#' @return DataFrame with columns "file" and "date".
#' @examples
#' \dontrun{
#' opp_dates <- get.opp.dates(db, opp_files)
#' }
#' @export
get.opp.dates <- function(db, opp_files) {
  sql <- "
    SELECT opp.file, sfl.date
    FROM opp
    INNER JOIN sfl ON opp.file == sfl.file
  "
  df <- sql.dbGetQuery(db, sql)
  idx <- match(opp_files, df$file)
  dates <- df[idx, "date"]
  df <- na.omit(data.frame("file"=opp_files, "date"=dates))
  return(df)
}

#' Get OPP file names for data with focused particles in all quantiles.
#'
#' @param db SQLite3 database file path.
#' @param all.files Return all OPP files that were considered for processing,
#'   even entries where the raw EVT was unreadable or no focused particles made
#'   it through filtering.
#' @param outliers If TRUE, remove data flagged as outliers
#' @return List of OPP file names based on the latest filtering
#'   parameters or NULL if no filtering has been done.
#' @examples
#' \dontrun{
#' opp.files <- get.opp.files(db)
#' }
#' @export
get.opp.files <- function(db, all.files=FALSE, outliers=TRUE) {
  check.for.populated.sfl(db)
  sql <- "
    SELECT DISTINCT opp.file
    FROM opp
    INNER JOIN sfl ON opp.file == sfl.file
  "
  if (! all.files) {
    # Only return files where all quantiles produced OPP data
    sql <- paste(sql, opp_quantile_inner_join_clause(), sep="\n")
  }
  if (outliers) {
    # Only return files not flagged as outliers
    sql2 <- paste0("
      INNER JOIN outlier ON sfl.file == outlier.file
      WHERE outlier.flag == ", FLAG_OK, "
    ")
    sql <- paste0(sql, sql2)
  }
  sql <- paste0(sql, "
    ORDER BY sfl.date ASC
  ")
  df <- sql.dbGetQuery(db, sql)
  return(df$file)
}

#' Save VCT aggregate population statistics for one file to vct table.
#'
#' @param db SQLite3 database file path.
#' @param vct_stats DataFrame of VCT statistics created by prep_vct_stats()
#' @return None
#' @examples
#' \dontrun{
#' save.vct.stats(db, vct_stats)
#' }
#' @export
save.vct.stats <- function(db, vct_stats) {
  sql.dbWriteTable(db, name="vct", value=vct_stats)
}

#' Save OPP aggregate statistics for one file/quantile combo to opp table.
#'
#' @param db SQLite3 database file path.
#' @param file.name File name with julian day directory.
#' @param evt_count Number of particles in EVT file.
#' @param opp OPP data frame with pop column.
#' @param filter.id ID for entry in filter table.
#' @return None
#' @examples
#' \dontrun{
#' save.opp.stats(db, "2014_185/2014-07-04T00-00-02+00-00",
#'                40000, opp, filter.params,
#'                "d3afb1ea-ad20-46cf-866d-869300fe17f4", 97.5)
#' }
#' @export
save.opp.stats <- function(db, file.name, all_count,
                           evt_count, opp, filter.id) {
  for (quantile in QUANTILES) {
    qcolumn <- paste0("q", quantile)

    if (nrow(opp)) {
      qopp <- opp[opp[qcolumn] == TRUE, ]  # opp for this quantile
    } else {
      qopp <- opp  # empty dataframe
    }

    qopp <- transformData(qopp)
    opp_count <- nrow(qopp)
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

  # Mark in outlier table as OK
  save.outliers(db, data.frame(file=clean.file.path(file.name), flag=FLAG_OK))
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
  for (i in 1:nrow(table.name)) {
    # Upsert!
    sql <- paste0("
      INSERT OR REPLACE INTO outlier(file,flag) VALUES('", table.name$file[i], "',", table.name$flag[i], ")
    ")
    sql.dbExecute(db, sql)
  }
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

  args <- c("db", "create", "-f")
  if (! is.null(cruise)) {
    args <- c(args, "-c", cruise)
  }
  if (! is.null(inst)) {
    args <- c(args, "-s", inst)
  }
  args <- c(args, normalizePath(sfl.file), normalizePath(db))

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
#' sql <- sfl_date_where_clause(sql, "2014-07-04 00:00", "2014-07-04 00:10")
#' sql <- sfl_date_where_clause(sql, "2014-07-04 00:00", "2014-07-04 00:10",
#'                             append=T)
#' }
sfl_date_where_clause <- function(start.date, end.date) {
  if (! is.null(start.date) || ! is.null(end.date)) {
    if (! is.null(start.date)) {
      start.date <- paste0("sfl.date >= '", date.to.db.date(start.date), "'")
    }
    if (! is.null(end.date)) {
      end.date <- paste0("sfl.date <= '", date.to.db.date(end.date), "'")
    }
    sql <- paste0("\n", paste(c(start.date, end.date), collapse=" AND "), "\n")
  } else {
    sql <- "\n"
  }
  return(sql);
}

#' Add an INNER JOIN to only select OPP files with data in all quantiles.
#'
#' Return a SQL string with an INNER JOIN to a subquery selecting for OPP files
#' that contain data in all quantiles. The benefit of making this a subquery is
#' the GROUP BY doesn't affect the rest of the SQL query this JOIN is embedded
#' into.
#'
#' @return SQL INNER JOIN string
#' @examples
#' \dontrun{
#' sql <- opp_quantile_inner_join_clause()
#' }
opp_quantile_inner_join_clause <- function() {
  sql <- "
  INNER JOIN
    (
      SELECT opp.file
      FROM opp
      WHERE opp.opp_count > 0
      GROUP BY opp.file
      HAVING count(opp.file) == 3
    ) AS opp_all_quantiles ON sfl.file == opp_all_quantiles.file
  "
  return(sql)
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
#' @param tables Tables to copy.
#' @return None
#' @examples
#' \dontrun{
#' copy_tables(db_from, db_to)
#' }
copy_tables <- function(db_from, db_to, tables) {
  # If dbs are the same file do nothing. This prevents erroneously erasing
  # tables then trying to copy from the just deleted tables.
  db_from <- normalizePath(db_from, mustWork=T)
  db_to <- normalizePath(db_to, mustWork=T)
  if (db_from == db_to) {
    print("Not copying db tables, db files are the same")
    return()
  }

  for (table_name in tables) {
    # Make sure columns match for table to copy
    col_from <- colnames(sql.dbGetQuery(db_from, paste0("SELECT * FROM ", table_name)))
    col_to <- colnames(sql.dbGetQuery(db_to, paste0("SELECT * FROM ", table_name)))
    if (! identical(col_from, col_to)) {
      stop(paste0("db files have differing columns for ", table_name, " table"))
    }

    # Clear the db_to table
    reset.table(db_to, table_name)

    # Get the db_from table
    table_from <- sql.dbGetQuery(db_from, paste0("select * from ", table_name))

    # Save to db_to table
    sql.dbWriteTable(db_to, name=table_name, value=table_from)
  }
}

#' Copy outlier entries from src_db to dest_db.
#'
#' This assumes db_to already has a complete outlier table with entries for
#' every file in the opp table. Entries in db_to with flag values in db_from
#' > 0 will be updated to reflect db_from. db_from entries without a
#' corresponding entry in db_to will be ignored.
#' @param db_from Popcycle database to copy flags > 0 from.
#' @param db_to Popcycle database to copy flags > 0 to.
#' @return None
#' @examples
#' \dontrun{
#' copy_outlier_table(db_from, db_to)
#' }
copy_outlier_table <- function(db_from, db_to) {
  db_from <- normalizePath(db_from, mustWork=T)
  db_to <- normalizePath(db_to, mustWork=T)
  # If dbs are the same file do nothing.
  if (db_from == db_to) {
    print("Not copying outlier tables, db files are the same")
    return()
  }

  src <- get.outlier.table(db_from)
  dest <- get.outlier.table(db_to)
  joined <- merge(x=src, y=dest, by="file", all.y=TRUE)
  # So we don't screw anything up and because merge may reorder rows by "by"
  # column, enforce a common sort order by "file" on both dataframes we'll use
  # going forward.
  dest <- dest[order(dest$file), ]
  joined <- joined[order(joined$file), ]

  new_dest_flags <- unlist(lapply(joined$flag.x, function(x) { if (is.na(x)) { 0 } else { x } }))
  # Check that new_dest_flags has the same number of values as dest here
  if (nrow(dest) != length(new_dest_flags)) {
    stop("copy_outlier_table produced an incorrect result")
  }
  dest$flag <- as.integer(new_dest_flags)
  reset.table(db_to, "outlier")
  sql.dbWriteTable(db_to, name="outlier", value=dest)
}

#' Get aggregate statistics data frame along with estimates of cell abundance.
#'
#' @param db SQLite3 database file path.
#' @param inst Instrument serial. If not provided will attempt to read from db.
#' @return Data frame of aggregate statistics.
#' @importFrom dplyr %>%
#' @export
get.stat.table <- function(db, inst=NULL) {
  if (is.null(inst)) {
    inst <- get.inst(db)
  }

  stat <- get.raw.stat.table(db)
  outliers <- get.outlier.table(db)

  #merge stat table with outlier table
  stat <- merge(stat, outliers, all.x=T)

  fr <- flowrate(stat$stream_pressure, inst=inst)

  stat[,"flow_rate"] <- fr[,1]
  stat[,"flow_rate_se"] <- fr[,2]

  # abundance is calculated based on a median value of opp_evt ratio for the entire cruise (volume of virtual core set for an entire cruise)
  qratios <- stat %>%
    dplyr::group_by(time, quantile) %>%
    dplyr::summarize(opp_evt_ratio=mean(opp_evt_ratio, na.rm=T)) %>%  # this just gets the single value per file,quantile which is duplicated for each pop
    dplyr::group_by(quantile) %>%  # now we have one ratio per file,quantile. group by quantile to create 3 groups with one ratio per file
    dplyr::summarize(opp_evt_ratio=median(opp_evt_ratio, na.rm=T))  # median of each quantile without double-counting for population duplicates

  for (q in qratios$quantile) {
    ratio <- qratios[qratios$quantile == q, "opp_evt_ratio"][[1]]
    qindex <- stat$quantile == q
    stat[qindex, c("abundance")]  <- stat[qindex, "n_count"] / (1000* ratio * stat[qindex, "flow_rate"] * stat[qindex, "file_duration"]/60)   # cells µL-1
    stat[qindex, c("abundance_se")]  <- stat[qindex, "abundance"] * stat[qindex, "flow_rate_se"] / stat[qindex, "flow_rate"]           # cells µL-1
  }

  # If Prochlorococcus or Synechococcus present, abundance is calculated based on individual opp_evt ratio (based on each file, not the median), since it provides more accurate results (see https://github.com/armbrustlab/seaflow-virtualcore)
  id <- which(stat$pop == "prochloro" | stat$pop == "synecho")
  if (length(id) > 0) {
    stat[id,c("abundance")]  <- stat[id,"n_count"] / (1000* stat[id,"opp_evt_ratio"] * stat[id,"flow_rate"] * stat[id,"file_duration"]/60)   # cells µL-1
    stat[id,c("abundance_se")]  <- stat[id,"abundance"] * stat[id,"flow_rate_se"] / stat[id,"flow_rate"]           # cells µL-1
  }

  return(stat)
}

#' Create particle size distribution
#'
#' @param db SQLite3 database file path.
#' @param vct.dir VCT file directory..
#' @param breaks Breaks must be a vector of values defining the breaks for the size distribution.
#' @param quantile OPP Filtering quantile.
#' @return Size distribution
#' @examples
#' \dontrun{
#'  
#' breaks <- # for Qc
#' min <- 0.003 # minimum quotas (3 fgC / cell)
#' delta <- 1/18 # to define the width of each bin
#' m <- 300 # number of bins
#' breaks <- round(min*2^(((1:m)-1)*delta),4) # log2 space bin
#' print(breaks)
#'
#' distribution <- create_PSD(db, vct.dir, breaks, quantile = 50)
#' }
#' @export 
create_PSD <- function(db, vct.dir, breaks, quantile = 50){

  QUANT <- as.numeric(quantile)

  # Get list of files, with list of outliers
  vct.list <- list.files(vct.dir, "parquet", full.names=T)
  
  ### 1. create PSD for each timepoint 
  i <- 1
  distribution <- tibble()
  for(file.name in vct.list){

    #file.name <- vct.list[2]
    message(round(100*i/length(vct.list)), "% completed \r", appendLF=FALSE)

    ## retrieve PSD data
    # Select data for QUANT
    if(QUANT == 2.5){
      vct <- arrow::read_parquet(file.name, col_select=c("date", c(!contains("diam") & contains('q2.5')))) %>% dplyr::filter(q2.5)
      vct <- dplyr::rename(vct, Qc_lwr = Qc_lwr_q2.5, Qc_mid = Qc_mid_q2.5, Qc_upr = Qc_upr_q2.5, pop = pop_q2.5)    
    }
    if(QUANT == 50){ 
      vct <- arrow::read_parquet(file.name, col_select=c("date", c(!contains("diam") & contains('q50')))) %>% dplyr::filter(q50)
      vct <- dplyr::rename(vct, Qc_lwr = Qc_lwr_q50, Qc_mid = Qc_mid_q50, Qc_upr = Qc_upr_q50, pop = pop_q50)    
    }
    if(QUANT == 97.5){
      vct <- arrow::read_parquet(file.name, col_select=c("date", c(!contains("diam") & contains('q97.5')))) %>% dplyr::filter(q97.5)
      vct <- dplyr::rename(vct, Qc_lwr = Qc_lwr_q97.5, Qc_mid = Qc_mid_q97.5, Qc_upr = Qc_upr_q97.5, pop = pop_q97.5)    
    }

    ## Get particle count in each bin 
    # group by population and by breaks
    # for each refractive index

    psd_lwr <-  vct %>% 
            dplyr::group_by(date, pop, breaks=cut(Qc_lwr, breaks), .drop=F) %>% 
            dplyr::count(breaks) %>%
            dplyr::pivot_wider(names_from = breaks, values_from = n) 
    psd_lwr <- psd_lwr %>% tibble::add_column(n='lwr', .after="pop")

    psd_mid <-  vct %>% 
            dplyr::group_by(date, pop=pop, breaks=cut(Qc_mid, breaks), .drop=F) %>% 
            dplyr::count(breaks) %>%
            dplyr::pivot_wider(names_from = breaks, values_from = n) 
    psd_mid <- psd_mid %>% tibble::add_column(n='mid', .after="pop")

    psd_upr <- vct %>% 
            dplyr::filter(pop != "beads") %>%
            dplyr::group_by(date, pop=pop, breaks=cut(Qc_upr, breaks), .drop=F) %>% 
            dplyr::count(breaks) %>%
            dplyr::pivot_wider(names_from = breaks, values_from = n) 
    psd_upr <- psd_upr %>% tibble::add_column(n='upr', .after="pop")

    # add data of each refractive index
    psd <- dplyr::bind_rows(psd_lwr, psd_mid, psd_upr)

    # bind data together
    distribution <- dplyr::bind_rows(distribution, psd)

    i <- i + 1
    flush.console()
  }

  ### 2. Retrieve metadata
  ## Retrieve SFL table
  sfl <- get.sfl.table(db)
  # format time
  sfl$time <- as.POSIXct(sfl$date, format="%FT%T", tz="UTC")
  # retrieve flow rate (mL min-1) of detectable volume
  fr <- flowrate(sfl$stream_pressure, inst= get.inst(db))$flow_rate
  # convert to microL min-1
  fr <- fr * 1000
  # acquisition time (min)
  acq.time <- sfl$file_duration/60
  # volume in microL
  sfl$volume <- round(fr * acq.time , 0)
    
  ## Retrive Outlier table
  outliers <- get.outlier.table(db)
  # merge with sfl
  sfl.all <- merge(sfl, outliers, by="file")

  ## Retrive OPP table
  # retrieve opp/evt
  opp <- as_tibble(get.opp.table(db))
  opp <- opp %>% dplyr::filter(quantile == QUANT)
    
  ## merge all metadata
  meta <- as_tibble(merge(sfl.all, opp, by="file")[c("time", "lat","lon","volume","opp_evt_ratio","flag")])
    
  ## merge metadata with PSD
  PSD <- as_tibble(merge(distribution, meta, by.x="date",by.y="time",all.x=T)) %>%
          relocate(contains("]"), .after=flag) # reorder column

  ### 3. calculate cell abundance in each bin (cells / microliter)
  ## calculate the volume of virtual core for each population, 
  # volume of virtual core is calculated based on a median value of opp_evt ratio for the entire cruise
  # except for small particles (i.e prochloro and synecho) where it is calcualted based on the opp_evt_ratio at that time
  id <- which(PSD$pop == "prochloro" | PSD$pop == "synecho")
  PSD[id, "volume"] <- PSD[id, "volume"] * PSD[id,"opp_evt_ratio"]
  PSD[-c(id), "volume"] <- PSD[-c(id), "volume"] * median(PSD[["opp_evt_ratio"]][-c(id)])
  
  ## calculate cell abundance in each bin (cells / microliter)
  clmn <- grep("]", names(PSD))
  PSD[,clmn] <- PSD[,clmn] / PSD[["volume"]]

return(PSD)
}


#' Manipulate the size distribution created by FCSplankton::create_PSD(). 
#' Calculate the sum of particles in each size class over specific temporal resolution; transform the header
#'
#' @param distribution Particle size disitribution created by FCSplankton::create_PSD().
#'  i.e., a tibble of size distribution over time. First column must be time (POSIXt class object);
#'  Second column must name of the population; other columns represent the different size classes. 
#'  Size classes can represent either diameter or carbon quota (assuming spherical particles).
#' @param time.step Time step over which to sum the number of particles in each size class. Default 1 hour, must be higher than 3 minutes
#' @param Qc.to.diam Convert carbon quotas to diameter as described in
#'  Menden-Deuer, S. and Lessard, E. J. Carbon to volume relationships for dinoflagellates, diatoms, and other protist plankton.
#'  Limnol. Oceanogr. 45, 569–579 (2000).
#' @param abundance.to.biomass Calculate carbon biomass in each population (i.e. cell abundance x Qc)
#' @param interval.to.geomean Transform size class intervals to geometric mean values 
#' (i.e. convert breaks (min, max] to geometric mean defined as sqrt(mean*max). 
#' @return Size distribution 
#' @name transform_PSD
#' @examples
#' \dontrun{
#' distribution <- transform_PSD(distribution, time.step="1 hour")
#' }
#' @export
transform_PSD <- function(distribution, time.step="1 hour", 
                                        Qc.to.diam=FALSE, 
                                        interval.to.geomean=FALSE,
                                        abundance.to.biomass=FALSE){
  
  # Check that 'time' is a POSIXt class object 
  if(! lubridate::is.POSIXt(distribution$date)){
  print("Date is not recognized as POSIXt class")
  stop
  }

  # Check that 'pop' column is there 
  if(!any(names(distribution)=='pop')){
    print("column 'pop' is missing")
  stop
  }

   # Calculate the mean in each size class over new time interval
  if(!is.null(time.step)){
    distribution <- distribution %>%
                      group_by(date = cut(date, breaks=time.step), pop) %>%
                      summarise(across(lat:lon, mean), across(volume, sum), across(contains("]"), sum))
  }                    



  # Menden-Deuer, S. & Lessard conversion factors
  d <- 0.261; e <- 0.860
  # select column that have PSD data
  clmn <- grep("]", names(distribution))
  # convert size interval (factors) into data.frame
  breaks <- strsplit(sub("\\]","",sub("\\(","",colnames(distribution)[clmn])),",")


  if(Qc.to.diam){
    #convert Qc into diam using the Menden-Deuer conversion
    b <- lapply(breaks, function(x) round(2*(3/(4*pi)*(as.numeric(x)/d)^(1/e))^(1/3),6))
    colnames(distribution)[clmn] <- sub("\\)","\\]", sub("c","",as.character(b)))
  }

  if(interval.to.geomean){
    # transform size class intervals to mean values (i.e. convert breaks (min, max] to geom mean). 
    if(Qc.to.diam){
      midval <- unlist(list(lapply(b, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
    }else{
      midval <- unlist(list(lapply(breaks, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
      }
    colnames(distribution)[clmn] <- round(midval,4)
  }
  
  if(abundance.to.biomass){
    # calculate biomass in each bin (pgC / L)
    midval <- unlist(list(lapply(breaks, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
    distribution[,clmn] <-  t(diag(midval) %*%  t(as.matrix(distribution[,clmn])))
  }


  # time converted to factor needs to be converted back to POSIXt
  distribution$date <- as.POSIXct(distribution$date, tz='GMT')

  return(distribution)

}

