delete.opp.stats.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM opp WHERE file == '", clean.file.path(file.name), "'")
  sql.dbGetQuery(db, sql)
}

delete.opp.by.file <- function(opp.dir, file.name) {
  opp.file <- paste0(opp.dir, "/", clean.file.path(file.name), ".opp")
  if (file.exists(opp.file)) {
    file.remove(opp.file)
  }
}

delete.vct.stats.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM vct WHERE file == '", clean.file.path(file.name), "'")
  sql.dbGetQuery(db, sql)
}

delete.vct.by.file <- function(vct.dir, file.name) {
  vct.file <- paste0(vct.dir, "/", clean.file.path(file.name), ".vct")
  if (file.exists(vct.file)) {
    file.remove(vct.file)
  }
}

delete.cytdiv.by.file <- function(db, file.name) {
  sql <- paste0("DELETE FROM cytdiv WHERE file == '", clean.file.path(file.name), "'")
  sql.dbGetQuery(db, sql)
}

delete.filter.params.by.uuid <- function(db, uuid) {
  sql <- paste0("DELETE FROM filter WHERE uuid == '", uuid, "'")
  sql.dbGetQuery(db, sql)
}

delete.gating.params.by.uuid <- function(db, uuid) {
  sql <- paste0("DELETE FROM gating WHERE uuid == '", uuid, "'")
  sql.dbGetQuery(db, sql)
  delete.poly.by.uuid(db, uuid)
}

delete.poly.by.uuid <- function(db, uuid) {
  sql <- paste0("DELETE FROM poly WHERE gating_uuid == '", uuid, "'")
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

get.opp.stats.table <- function(db) {
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

get.opp.stats.by.date <- function(db, start.date=NULL, end.date=NULL) {
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

get.vct.stats.by.date <- function(db, start.date=NULL, end.date=NULL) {
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

get.vct.stats.table <- function(db) {
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

get.opp.by.date <- function(db, opp.dir, start.date, end.date, channel=NULL,
                            transform=TRUE, vct.dir=NULL, pop=NULL) {
  opp.stats <- get.opp.stats.by.date(db, start.date=start.date, end.date=end.date)
  opp <- get.opp.by.file(opp.dir, opp.stats$file, channel=channel,
                         transform=transform, vct.dir=vct.dir, pop=pop)
  return(opp)
}

# file.name can be a single file path or a list of file paths. If more than
# one file is used a concatenated opp data frame will be returned.
get.opp.by.file <- function(opp.dir, file.name, channel=NULL,
                            transform=TRUE, vct.dir=NULL, pop=NULL) {
  # OPP files are never gzipped so it's OK to remove .gz here
  file.name.clean <- unlist(lapply(file.name, clean.file.path))
  opp.files <- paste0(file.name.clean, ".opp")
  opp.reader <- function(f) {
    opp <- readSeaflow(f, opp.dir, channel=channel, transform=transform)
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

get.vct.by.file <- function(vct.dir, file.name) {
  vct.file <- paste0(vct.dir, "/", clean.file.path(file.name), ".vct")
  vct <- read.table(vct.file, col.names=c("pop"))
  return(vct)
}

get.sfl.table <- function(db) {
  # Don't check for populated SFL table here since it should be obvious
  # if it's populated by result. Also, this would lead to infinite recursion
  # since check.for.populated.sfl calls this function.
  sql <- "SELECT * FROM sfl ORDER BY date ASC"
  sfl <- sql.dbGetQuery(db, sql)
  return(sfl)
}

# Get SFL rows >= start.date and <= end.date
#
# Args:
#   db: sqlite database
#   start.date: start date in format YYYY-MM-DD HH:MM
#   end.date:   end date in format YYYY-MM-DD HH:MM
get.sfl.by.date <- function(db, start.date, end.date) {
  check.for.populated.sfl(db)
  sql <- "SELECT * FROM sfl"
  sql <- sfl_date_where_clause(sql, start.date, end.date, append=F)
  sfl <- sql.dbGetQuery(db, sql)
  return(sfl)
}

get.filter.params.latest <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date DESC LIMIT 1"
  result <- sql.dbGetQuery(db, sql)
  return(result)
}

get.filter.params.by.uuid <- function(db, uuid) {
  sql <- paste0("SELECT * FROM filter WHERE uuid = ", uuid)
  result <- sql.dbGetQuery(db, sql)
  return(result)
}

get.filter.table <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date ASC"
  result <- sql.dbGetQuery(db, sql)
  return(result)
}

# Get latest gating parameters as two item named list
#
# list$row contains the dataframe for the latest gating table entry
# list$poly.log contains the list of polygon coordinates for each population
get.gating.params.latest <- function(db) {
  sql <- "SELECT * FROM gating ORDER BY date DESC LIMIT 1"
  gating.df <- sql.dbGetQuery(db, sql)
  poly.log <- poly.log.from.db.gating.df(db, gating.df)
  answer <- list(row=gating.df, poly.log=poly.log)
  return(answer)
}

# Get gating parameters by uuid
get.gating.params.by.uuid <- function(db, uuid) {
  sql <- paste0("SELECT * FROM gating WHERE uuid = '", uuid, "'")
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
  for (i in seq(length(pop.names))) {
    sql <- paste0("
      SELECT * FROM poly
      WHERE
        gating_uuid = '", gating.df$uuid[1], "'
        AND
        pop = '", pop.names[i], "'"
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
    pop.poly[, "gating_uuid"] <- NULL
    poly.log[[i]] <- as.matrix(pop.poly)
  }
  names(poly.log) <- pop.names
  return(poly.log)
}

get.gating.table <- function(db) {
  sql <- "SELECT * FROM gating ORDER BY date ASC"
  gating <- sql.dbGetQuery(db, sql)
  return(gating)
}

get.poly.table <- function(db) {
  sql <- "SELECT * FROM poly ORDER BY gating_uuid, pop ASC"
  poly <- sql.dbGetQuery(db, sql)
  return(poly)
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
  evt.list <- unlist(lapply(evt.list, function(f) { clean.file.path(f) }))
  return(setdiff(evt.list, opp.files))
}

get.stat.table <- function(db) {
  check.for.populated.sfl(db)
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
    time, pop ASC;)"

  stats <- sql.dbGetQuery(db, sql)
  return (stats)
}

get.cytdiv.table <- function(db) {
  check.for.populated.sfl(db)
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
               value = data.frame(cruise = cruise.name, file = clean.file.path(file.name),
                                  N0 = indices[1], N1= indices[2], H=indices[3],
                                  J=indices[4], opp_red=indices[5]),
               row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

save.vct.stats <- function(db, opp, cruise.name, file.name, method,
                           gating_uuid) {
  df <- ddply(opp, .(pop), here(summarize),
              cruise=cruise.name, file=clean.file.path(file.name),
              count=length(pop), method=method,
              fsc_small=mean(fsc_small), fsc_perp=mean(fsc_perp),
              chl_small=mean(chl_small), pe=mean(pe), gating_uuid=gating_uuid)
  cols <- c("cruise", "file", "pop", "count", "method", "fsc_small",
            "fsc_perp", "pe", "chl_small", "gating_uuid")
  df.reorder <- df[cols]
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="vct", value=df.reorder, row.names=F, append=T)
  dbDisconnect(con)
}

save.vct.file <- function(vct, vct.dir, file.name) {
  vct.file <- paste0(vct.dir, "/", clean.file.path(file.name), ".vct")
  dir.create(dirname(vct.file), showWarnings=F, recursive=T)
  write.table(vct, vct.file, row.names=F, col.names=F, quote=F)
}

save.opp.stats <- function(db, cruise.name, file.name, evt_count, opp, params,
                           filter_uuid) {
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
                   filter_uuid=filter_uuid
        )
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="opp", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

save.opp.file <- function(opp, opp.dir, file.name) {
  opp.file <- paste0(opp.dir, "/", clean.file.path(file.name), ".opp")
  dir.create(dirname(opp.file), showWarnings=F, recursive=T)
  writeSeaflow(opp, opp.file, linearize=FALSE)
}

save.filter.params <- function(db, params) {
  uuid <- UUIDgenerate()  # create primary ID for new entry
  date.stamp <- format(Sys.time(),format="%FT%H:%M:%S+0000", tz="GMT")
  df <- data.frame(uuid=uuid, date=date.stamp, notch1=params$notch1,
                   notch2=params$notch2, offset=params$offset,
                   origin=params$origin, width=params$width)
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="filter", value=df, row.names=F, append=T)
  dbDisconnect(con)
}

save.gating.params <- function(db, poly.log, new.entry=FALSE) {
  if (new.entry) {
    uuid <- UUIDgenerate()  # create primary ID for new entry
    date.stamp <- format(Sys.time(),format="%FT%H:%M:%S+0000", tz="GMT")
  } else {
    gating.df <- get.gating.params.latest(db)$row
    if (nrow(gating.df) == 0) {
      stop("new.entry=FALSE in upload.gating, but no gating entry to update")
    }
    uuid <- gating.df[1, "uuid"] # get primary ID for entry to be replaced
    date.stamp <- gating.df[1, "date"]
    delete.gating.params.by.uuid(db, uuid) # erase entry to be replaced
  }

  df <- data.frame(uuid=uuid, date=date.stamp,
                   pop_order=paste(names(poly.log), collapse=","))

  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="gating", value=df, row.names=F, append=T)
  dbDisconnect(con)

  save.poly(db, poly.log, uuid)
}

save.poly <- function(db, poly.log, gating_uuid) {
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
  df$gating_uuid <- gating_uuid  # last field in table

  delete.poly.by.uuid(db, gating_uuid)
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

check.for.populated.sfl <- function(db) {
  if (nrow(get.sfl.table(db)) == 0) {
    stop("SFL table is empty")
  }
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
