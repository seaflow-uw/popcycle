delete.opp.stats.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM opp WHERE file == '", remove.gz(file.name), "'")
  sql.dbGetQuery(db, sql)
}

delete.vct.stats.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM vct WHERE file == '", remove.gz(file.name), "'")
  sql.dbGetQuery(db, sql)
}

delete.cytdiv.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM ctydiv WHERE file == '", remove.gz(file.name), "'")
  sql.dbGetQuery(db, sql)
}

delete.filter.by.id <- function(db, id) {
  sql <- paste0("DELETE FROM filter WHERE id == '", id, "'")
  sql.dbGetQuery(db, sql)
}

delete.gating.by.uuid <- function(db, uuid.str) {
  sql <- paste0("DELETE FROM gating WHERE uuid == '", uuid.str, "'")
  sql.dbGetQuery(db, sql)
}

delete.poly.by.uuid <- function(db, uuid.str) {
  sql <- paste0("DELETE FROM poly WHERE gating_uuid == '", uuid.str, "'")
  sql.dbGetQuery(db, sql)
}

reset.opp <- function(db) {
  reset.table(db, "opp")
}

reset.vct <- function(db) {
  reset.table(db, "vct")
}

reset.cytdiv <- function(db) {
  reset.table(db, "cytdiv")
}

reset.sfl <- function(db) {
  reset.table(db, "sfl")
}

reset.filter <- function(db) {
  reset.table(db, "filter")
}

reset.gating <- function(db) {
  reset.table(db, "gating")
}

reset.poly <- function(db) {
  reset.table(db, "poly")
}

reset.table <- function(db, table.name) {
  sql <- paste0("DELETE FROM ", table.name)
  sql.dbGetQuery(db, sql)
}

# Remove entries for all tables except filter, gating, and poly
reset.db <- function(db) {
  reset.opp(db)
  reset.vct(db)
  reset.cytdiv(db)
  reset.sfl(db)
}

get.opp.stats.by.file <- function(db, file.name) {
  sql <- paste0("SELECT
    sfl.date, opp.*
  FROM
    sfl, opp
  WHERE
    opp.file == '", remove.gz(file.name), "'
    AND
    sfl.cruise == opp.cruise
    AND
    sfl.file == opp.file")
  opp <- sql.dbGetQuery(db, sql)
  return(opp)
}

get.opp.stats.table <- function(db) {
  sql <- "
    SELECT
      sfl.date, opp.*
    FROM
      sfl, opp
    WHERE
      sfl.cruise == opp.cruise
      AND
      sfl.file == opp.file"
  opp <- sql.dbGetQuery(db, sql)
  return(opp)
}

get.opp.stats.by.date <- function(db, start.date=NULL, end.date=NULL) {
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

get.vct.stats.by.date <- function(db, start.date=NULL, end.date=NULL) {
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

get.vct.stats.table <- function(db) {
  sql <- "
    SELECT
      sfl.date, vct.*
    FROM
      sfl, vct
    WHERE
      sfl.cruise == vct.cruise
      AND
      sfl.file == vct.file"
  vct <- sql.dbGetQuery(db, sql)
  return(vct)
}

get.opp.by.file <- function(file.name, channel=NULL, opp.dir=NULL,
                            transform=TRUE) {
  # OPP files are never gzipped so it's OK to remove .gz here
  opp.file <- paste0(remove.gz(file.name), ".opp")
  opp <- readSeaflow(opp.file, opp.dir, channel=channel,
                     transform=transform)
  return(opp)
}

get.vct.by.file <- function(file.name, vct.dir=NULL) {
  vct.file <- paste0(vct.dir, "/", remove.gz(file.name), ".vct")
  vct <- read.table(vct.file, col.names=c("pop"))
  return(vct)
}

get.opp.by.date <- function(db, start.date, end.date, pop=NULL, channel=NULL,
                            transform=TRUE, opp.dir=NULL, vct.dir=NULL) {
  opp.stats <- get.opp.stats.by.date(db, start.date=start.date, end.date=end.date)
  opp.reader <- function(f) {
    opp <- get.opp.by.file(f, opp.dir, channel=channel, transform=transform)
    return(opp)
  }
  opps <- lapply(opp.stats$file, opp.reader)
  opps.bound <- rbind.fill(opps)

  if (! is.null(vct.dir)) {
    vcts <- lapply(opp.stats$file, function(f) get.vct.by.file(f, vct.dir))
    vcts.bound <- rbind.fill(vcts)
    opps.bound <- cbind(opps.bound, vcts.bound)
    if (! is.null(pop)) {
      opps.bound <- opps.bound[opps.bound$pop == pop, ]
    }
  }

  return(opps.bound)
}

# Get SFL rows >= start.date and <= end.date
#
# Args:
#   db: sqlite database
#   start.date: start date in format YYYY-MM-DD HH:MM
#   end.date:   end date in format YYYY-MM-DD HH:MM
get.sfl <- function(db, start.date=NULL, end.date=NULL) {
  sql <- "SELECT * FROM sfl"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=F)
  sfl <- sql.dbGetQuery(db, sql)
  return(sfl)
}

get.filter.latest <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date DESC LIMIT 1"
  result <- sql.dbGetQuery(db, sql)
  result[, "id"] <- NULL
  result[, "date"] <- NULL
  return(result)
}

get.filter.by.id <- function(db, id) {
  sql <- paste0("SELECT * FROM filter WHERE id = ", id)
  result <- sql.dbGetQuery(db, sql)
  result[, "id"] <- NULL
  result[, "date"] <- NULL
  return(result)
}

get.filter.table <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date ASC"
  result <- sql.dbGetQuery(db, sql)
  return(result)
}

# Get latest gating parameters
get.gating.latest <- function(db) {
  if (is.null(uuid)) {
    sql <- "SELECT * FROM gating ORDER BY date DESC LIMIT 1"
  } else {
    sql <- paste0("SELECT * FROM gating WHERE uuid = '", uuid, "'")
  }
  gating.df <- sql.dbGetQuery(db, sql)
  poly.log <- poly.log.from.db.gating.df(db, gating.df)
  return(poly.log)
}

# Get gating parameters by uuid
get.gating.by.uuid <- function(db, uuid) {
  sql <- "SELECT * FROM gating ORDER BY date DESC LIMIT 1"
  gating.df <- sql.dbGetQuery(db, sql)
  poly.log <- poly.log.from.db.gating.df(db, gating.df)
  return(poly.log)
}

poly.log.from.db.gating.df <- function(db, gating.df) {
  if (nrow(gating.df) == 0) {
    return(data.frame())
  } else if (nrow(gating.df) > 1) {
    stop("gating.df must contain at most one entry")
  }

  poly.log <- list()
  pop.names <- strsplit(gating.df$pop_order, ",")[[1]]
  channels <- c("fsc_small", "chl_small", "pe", "fsc_big", "chl_big")
  for (i in seq(length(pop.names))) {
    sql <- paste0("
      SELECT * FROM poly
      WHERE
        gating_uuid = '", gating.df$uuid[1], "'
        AND
        pop = '", pop.names[i], "'"
    )
    pop.poly <- sql.dbGetQuery(db, sql)
    for (c in channels) {
      if (all(is.na(pop.poly[, c]))) {
        pop.poly[, c] <- NULL
      }
    }
    pop.poly[, "pop"] <- NULL
    pop.poly[, "gating_uuid"] <- NULL
    poly.log[[i]] <- as.matrix(pop.poly)
  }
  names(poly.log) <- pop.names
}

get.gating.table <- function(db) {
  sql <- "SELECT * FROM gating ORDER BY date ASC"
  gating <- sql.dbGetQuery(db, sql)
  return(gating)
}

# Return a vector of distinct file values in opp table
#
# Args:
#   db = sqlite3 db
get.opp.files <- function(db) {
  sql <- "SELECT DISTINCT file from opp"
  files <- sql.dbGetQuery(db, sql)
  print(paste(length(files$file), "opp files found"))
  return(files$file)
}

# Return a vector of distinct file values in vct table
#
# Args:
#   db = sqlite3 db
get.vct.files <- function(db) {
  sql <- "SELECT DISTINCT file from vct"
  files <- sql.dbGetQuery(db, sql)
  return(files$file)
}

# Return a list of EVT files for which there is no OPP data in the database
#
# Args:
#   evt.list = list of EVT file paths, e.g. get.evt.list(evt.location)
#   db = sqlite3 db
get.empty.evt.files <- function(db, evt.list) {
  opp.files <- get.opp.files(db)
  evt.list <- unlist(lapply(evt.list, function(f) { remove.gz(f) }))
  return(setdiff(evt.list, opp.files))
}

get.stat.table <- function(db) {
  sql <- "
  SELECT
    opp.cruise as cruise,
    opp.file as file,
    sfl.date as time,
    sfl.lat as lat,
    sfl.lon as lon,
    opp.opp_evt_ratio as opp_evt_ratio,
    sfl.flow_rate as flow_rate,
    sfl.file_duration as file_duration,
    vct.pop as pop,
    vct.count as n_count,
    vct.count / (sfl.flow_rate * (sfl.file_duration/60) * opp.opp_evt_ratio) as abundance,
    vct.fsc_small as fsc_small,
    vct.chl_small as chl_small,
    vct.pe as pe
  FROM
    opp, vct, sfl
  WHERE
    opp.cruise == vct.cruise
    AND
    opp.file == vct.file
    AND
    opp.cruise == sfl.cruise
    AND
    opp.file == sfl.file
  ORDER BY
    time, pop;)"

  stats <- sql.dbGetQuery(db, sql)
  return (stats)
}

get.cytdiv.table <- function(db) {
  sql <- "SELECT
            sfl.cruise as cruise,
            sfl.file as file,
            sfl.date as time,
            sfl.lat as lat,
            sfl.lon as lon,
            cytdiv.N0 as N0,
            cytdiv.N1 as N1,
            cytdiv.H as H,
            cytdiv.J as J,
            cytdiv.opp_red as opp_red,
            sfl.bulk_red as bulk_red
           FROM sfl, cytdiv
           WHERE
            sfl.cruise == cytdiv.cruise
            AND
            sfl.file == cytdiv.file
            ORDER BY time ASC ;"
  cytdiv <- sql.dbGetQuery(db, sql)
  return (cytdiv)
}

save.cytdiv <- function(db, indices, cruise.name, file.name) {
  con <- dbConnect(SQLite(), dbname = db)
  dbWriteTable(conn = con, name = "cytdiv",
               value = data.frame(cruise = cruise.name, file = remove.gz(file.name),
                                  N0 = indices[1], N1= indices[2], H=indices[3],
                                  J=indices[4], opp_red=indices[5]),
               row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

save.vct.stats <- function(db, opp, cruise.name, file.name, method) {
  df <- ddply(opp, .(pop), here(summarize),
              cruise=cruise.name, file=remove.gz(file.name),
              count=length(pop), method=method,
              fsc_small=mean(fsc_small), fsc_perp=mean(fsc_perp),
              chl_small=mean(chl_small), pe=mean(pe))
  cols <- c("cruise", "file", "pop", "count", "method", "fsc_small",
            "fsc_perp", "pe", "chl_small")
  df.reorder <- df[cols]
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="vct", value=df.reorder, row.names=F, append=T)
  dbDisconnect(con)
}

save.vct.file <- function(vct.dir, evt.file, vct) {
  vct.file <- paste0(vct.dir, "/", remove.gz(evt.file), ".vct")
  dir.create(dirname(vct.file), showWarnings=F, recursive=T)
  write.table(vct, vct.file, row.names=F, col.names=F, quote=F)
}

save.opp.stats <- function(db, cruise.name, file.name, evt_count, opp, params) {
  if (nrow(opp) == 0) {
    return
  }
  opp <- transformData(opp)
  opp_count <- nrow(opp)
  df <- data.frame(cruise=cruise.name, file=remove.gz(file.name),
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
                   chl_big_mean=mean(opp$chl_big)
        )
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="opp", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

save.opp.file <- function(opp.dir, evt.file , opp) {
  opp.file <- paste0(opp.dir, "/", remove.gz(evt.file), ".opp")
  dir.create(dirname(opp.file), showWarnings=F, recursive=T)
  writeSeaflow(opp, opp.file, linearize=FALSE)
}

save.filter <- function(db, params) {
  date.stamp <- format(Sys.time(),format="%FT%H:%M:%S+0000", tz="GMT")
  df <- data.frame(id=NA, date=date.stamp, notch1=params$notch1,
                   notch2=params$notch2, offset=params$offset,
                   origin=params$origin, width=params$width)
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="filter", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

save.gating <- function(db, poly.log, new.entry=FALSE) {
  if (new.entry) {
    uuid.str <- UUIDgenerate()  # create primary ID for new entry
    date.stamp <- format(Sys.time(),format="%FT%H:%M:%S+0000", tz="GMT")
  } else {
    gating.table <- get.gating.table(db)
    if (nrow(gating.table) == 0) {
      stop("new.entry=FALSE in upload.gating, but no gating entry to update")
    }
    uuid.str <- gating.table[1, "uuid"] # get primary ID for entry to be replaced
    date.stamp <- gating.table[1, "date"]
    delete.gating.by.uuid(db, uuid.str) # erase entry to be replaced
  }

  df <- data.frame(date=date.stamp, uuid=uuid.str,
                   pop_order=paste(names(poly.log), collapse=","))

  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="gating", value=df, row.names=F, append=T)
  dbDisconnect(con)

  save.poly(db, poly.log, uuid.str)
}

save.poly <- function(db, poly.log, uuid.str) {
  ns <- names(poly.log)
  df <- data.frame()
  channels <- c("fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small",
                "chl_big")

  if (length(ns) == 0) {
    return()
  }
  for (i in seq(length(ns))) {
    p <- poly.log[[ns[i]]]
    tmpdf <- data.frame(uuid=rep(uuid.str, nrow(p)))
    tmpdf[, "pop"] <- ns[i]
    for (col in channels) {
      tmpdf[, col] <- NA
    }
    for (col in colnames(p)) {
      tmpdf[, col] <- p[, col]
    }
    df <- rbind(df, tmpdf)
  }

  delete.poly.by.uuid(db, uuid.str)

  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="poly", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

# Create a new, empty sqlite3 database using schema from original db
#
# Args:
#   new.db.path = path to a new sqlite3 database
make.sqlite.db <- function(new.db.path) {
  sql.file <- paste(system.file("sql", package="popcycle"), "popcycle.sql", sep="/")
  cmd <- sprintf("sqlite3 %s < %s", new.db.path, sql.file)
  status <- system(cmd)
  if (status > 0) {
    stop(paste("Db creation command '", cmd, "' failed with exit status ", status))
  }
}

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

# Convert a date string in format YYYY-MM-DD HH:MM to format suitable for db
# date field comparison.
#
# Args:
#   date.string: In format YYYY-MM-DD HH:MM
date.to.db.date <- function(date.string) {
  return(POSIXct.to.db.date(string.to.POSIXct(date.string)))
}

# Returns a POSIXct object for a human readable date string
#
# Args:
#   date.string: In format YYYY-MM-DD HH:MM
string.to.POSIXct <- function(date.string) {
  # Make POSIXct objects in GMT time zone
  date.ct <- as.POSIXct(strptime(date.string, format="%Y-%m-%d %H:%M", tz="GMT"))
  if (is.na(date.ct)) {
    stop(paste("wrong format for date.string parameter : ", date.string,
               "instead of ", "%Y-%m-%d %H:%M"))
  }
  return(date.ct)
}

# Convert a POSIXct date into a string suitable for comparisons in db date
# fields comparison.
POSIXct.to.db.date <- function(date.ct) {
  return(format(date.ct, "%Y-%m-%dT%H:%M:00"))
}
