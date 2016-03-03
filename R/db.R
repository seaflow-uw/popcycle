library(RSQLite)

.delete.opp.stats.by.file <- function(file.name, db=db.name) {
  sql <- paste0("DELETE FROM opp WHERE file == '", file.name, "'")
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.vct.stats.by.file <- function(file.name, db=db.name) {
  sql <- paste0("DELETE FROM vct WHERE file == '", file.name, "'")
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.cytdiv.by.file <- function(file.name, db=db.name) {
  sql <- paste0("DELETE FROM ", cytdiv.table.name, " WHERE file == '",
                file.name, "'")
  con <- dbConnect(SQLite(), dbname=db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.sfl <- function(db=db.name) {
  sql <- paste0("DELETE FROM ", sfl.table.name)
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

get.opp.stats.by.file <- function(file.name, db=db.name) {
  sql <- paste0("SELECT
    sfl.date, opp.*
  FROM
    sfl, opp
  WHERE
    opp.file == '", file.name, "'
    AND
    sfl.cruise == opp.cruise
    AND
    sfl.file == opp.file")
  con <- dbConnect(SQLite(), dbname = db)
  opp <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(opp)
}

get.opp.stats.by.date <- function(start.date, end.date, db=db.name) {
  date.bounds <- c(date.to.db.date(start.date), date.to.db.date(end.date))
  sql <- paste0("SELECT
    sfl.date, opp.*
  FROM
    sfl, opp
  WHERE
    sfl.date >= '", date.bounds[1], "'
    AND
    sfl.date <= '", date.bounds[2], "'
    AND
    sfl.cruise == opp.cruise
    AND
    sfl.file == opp.file")
  con <- dbConnect(SQLite(), dbname = db)
  opp <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(opp)
}

get.opp.by.file <- function(file.name, channel=NULL, opp.dir=NULL,
                            transform=TRUE) {
  opp <- readSeaflow(file.name, opp.dir, channel=channel, transform=transform)
  return(opp)
}

get.vct.by.file <- function(file.name, vct.dir=NULL) {
  vct.file <- paste0(vct.dir, "/", file.name, ".vct")
  vct <- read.table(vct.file, col.names=c("pop"))
  return(vct)
}

get.opp.by.date <- function(start.time, end.time, pop=NULL, channel=NULL,
                            transform=TRUE, opp.dir=NULL, vct.dir=NULL,
                            db=db.name) {
  dates <- get.opp.stats.by.date(start.time, end.time, db)
  opp.reader <- function(f) {
    opp <- get.opp.by.file(f, opp.dir, channel=channel, transform=transform)
    return(opp)
  }
  opps <- lapply(dates$file, opp.reader)
  opps.bound <- rbind.fill(opps)

  if (! is.null(vct.dir)) {
    vcts <- lapply(dates$file, function(f) get.vct.by.file(f, vct.dir))
    vcts.bound <- rbind.fill(vcts)
    opps.bound <- cbind(opps.bound, vcts.bound)
    if (! is.null(pop)) {
      opps.bound <- opps.bound[opps.bound$pop == pop, ]
    }
  }

  return(opps.bound)
}

# Return a list of min and max values for each of opp channels
# The list contains a named member for each channel (e.g. x$fsc_small),
# and each member is a two item vector of min and max values (e.g. c(1, 1000))
#
# To retrieve the min value for fsc_small from the list:
# x$fsc_small[1]
# To retriev ethe max value for fsc_small from the list:
# x$ fsc_small[2]
get.opp.channel.ranges <- function(db=db.name) {
  con <- dbConnect(SQLite(), dbname=db)
  minmaxes = list()
  channels <- c("fsc_small", "fsc_big", "fsc_perp", "pe", "chl_small", "chl_big")
  for (channel in channels) {
    sql <- paste0("SELECT MIN(", channel, "_min) FROM opp")
    min.answer <- dbGetQuery(con, sql)
    sql <- paste0("SELECT MAX(", channel, "_max) FROM opp")
    max.answer <- dbGetQuery(con, sql)
    minmaxes[[channel]] = c(min.answer[1,1], max.answer[1,1])
  }
  dbDisconnect(con)
  return(minmaxes)
}

# Get SFL rows >= start.date and <= end.date
#
# Args:
#   start.date: start date in format YYYY-MM-DD HH:MM
#   end.date:   end date in format YYYY-MM-DD HH:MM
get.sfl.by.date <- function(start.date, end.date, db=db.name) {
  date.bounds <- c(date.to.db.date(start.date), date.to.db.date(end.date))

  con <- dbConnect(SQLite(), dbname = db)
  sql <- paste0("SELECT * FROM ", sfl.table.name,
                " WHERE date >= '", date.bounds[1], "'",
                " AND", " date <= '", date.bounds[2], "'")
  sfl <- dbGetQuery(con, sql)
  return(sfl)
}

upload.vct <- function(opp, cruise.name, file.name, method, db=db.name) {
  check.cruise.id(cruise.name)

  df <- ddply(opp, .(pop), here(summarize),
              cruise=cruise.name, file=file.name, count=length(pop), method=method,
              fsc_small=mean(fsc_small), chl_small=mean(chl_small), pe=mean(pe))
  cols <- c("cruise", "file", "pop", "count", "method", "fsc_small",
            "chl_small", "pe")
  df.reorder <- df[cols]
  con <- dbConnect(SQLite(), dbname=db)
  dbWriteTable(conn=con, name="vct", value=df.reorder, row.names=F, append=T)
  dbDisconnect(con)
}

save.vct.file <- function(vct, opp.file, vct.dir) {
  vct.file <- paste0(vct.dir, "/", opp.file, ".vct")
  dir.create(dirname(vct.file), showWarnings=F, recursive=T)
  write.table(vct, vct.file, row.names=F, col.names=F, quote=F)
}

# Return a vector of distinct file values in opp table
#
# Args:
#   db = sqlite3 db
get.opp.files <- function(db=db.name) {
  sql <- "SELECT DISTINCT file from opp"
  con <- dbConnect(SQLite(), dbname=db)
  files <- dbGetQuery(con, sql)
  dbDisconnect(con)
  print(paste(length(files$file), "opp files found"))
  return(files$file)
}

# Return a vector of distinct file values in vct table
#
# Args:
#   db = sqlite3 db
get.vct.files <- function(db=db.name) {
  sql <- "SELECT DISTINCT file from vct"
  con <- dbConnect(SQLite(), dbname=db)
  files <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(files$file)
}

# Return a list of EVT files for which there is no OPP data in the database
#
# Args:
#   evt.list = list of EVT file paths, e.g. get.evt.list(evt.location)
#   db = sqlite3 db
get.empty.evt.files <- function(evt.list, db=db.name) {
  opp.files <- get.opp.files(db)
  return(setdiff(evt.list, opp.files))
}

get.stat.table <- function(db = db.name) {
  sql <- paste('SELECT * FROM ', stats.table.name, 'ORDER BY time ASC')
  con <- dbConnect(SQLite(), dbname = db)
  stats <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return (stats)
}

insert.stats.for.file <- function(file.name, db = db.name) {
  # [TODO Francois] Name of OPP, vct, sfl, opp_evt_ratio tables should be a variable too.
  sql <- "INSERT INTO stats
SELECT
  opp.cruise as cruise,
  opp.file as file,
  sfl.date as time,
  sfl.lat as lat,
  sfl.lon as lon,
  opp_evt_ratio.ratio as opp_evt_ratio,
  sfl.flow_rate as flow_rate,
  sfl.file_duration as file_duration,
  vct.pop as pop,
  count(vct.pop) as n_count,
  count(vct.pop) / (sfl.flow_rate * (sfl.file_duration/60) * opp_evt_ratio.ratio) as abundance,
  avg(opp.fsc_small) as fsc_small,
  avg(opp.chl_small) as chl_small,
  avg(pe) as pe
FROM
  opp, vct, sfl, opp_evt_ratio
WHERE
  opp.cruise == vct.cruise
  AND
  opp.file == vct.file
  AND
  opp.particle == vct.particle
  AND
  opp.cruise == sfl.cruise
  AND
  opp.file == sfl.file
  AND
  opp.cruise == opp_evt_ratio.cruise
  AND
  opp.file == opp_evt_ratio.file
  AND
  opp.file == 'FILE_NAME'
GROUP BY
  opp.cruise, opp.file, vct.pop;"

  #in case there's stats in there already
  sql.delete <- gsub('FILE_NAME', file.name, paste('DELETE FROM', stats.table.name, 'WHERE file == "FILE_NAME"'))
  con <- dbConnect(SQLite(), dbname = db)
  response <- dbGetQuery(con, sql.delete)

  sql <- gsub('FILE_NAME', file.name, sql)
  response <- dbGetQuery(con, sql)
  dbDisconnect(con)
}


run.stats <- function(opp.list, db=db.name){

  # delete old stats entries if they exist so we keep cruise/file distinct
  .delete.stats()

  i <- 0
  for (opp.file in opp.list) {

     message(round(100*i/length(opp.list)), "% completed \r", appendLF=FALSE)

    tryCatch({
    #   print('Updating stat')
      insert.stats.for.file(opp.file, db=db.name)
    }, error = function(e) {print(paste("Encountered error with file", opp.file))})

    i <-  i + 1
    flush.console()

  }
}



upload.cytdiv <- function(indices, cruise.name, file.name, db = db.name) {
  check.cruise.id(cruise.name)

  con <- dbConnect(SQLite(), dbname = db)
  dbWriteTable(conn = con, name = cytdiv.table.name,
               value = data.frame(cruise = cruise.name, file = file.name, N0 = indices[1], N1= indices[2], H=indices[3], J=indices[4], opp_red=indices[5]),
               row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}


get.cytdiv.table <- function(db = db.name) {
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

  con <- dbConnect(SQLite(), dbname = db)
  cytdiv <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return (cytdiv)
}


get.sfl.table <- function(db=db.name) {
  sql <- paste('SELECT * FROM ', sfl.table.name, 'ORDER BY date ASC')
  con <- dbConnect(SQLite(), dbname=db)
  sfl <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(sfl)
}

get.opp.stats.table <- function(db=db.name) {
  sql <- paste('SELECT * FROM opp')
  con <- dbConnect(SQLite(), dbname=db)
  opp <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(opp)
}

get.vct.table <- function(db=db.name) {
  sql <- paste('SELECT * FROM vct')
  con <- dbConnect(SQLite(), dbname=db)
  opp <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(opp)
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

# Merge opp, opp.evt.ratio, vct tables from multiple sqlite dbs into target.db.
# Erase files in src.dbs once merged.
#
# Args:
#   src.dbs: paths of sqlite3 dbs to merge into target.db
#   target.db: path of sqlite3 db to be merged into
merge.dbs <- function(src.dbs, target.db=db.name) {
  for (src in src.dbs) {
    # First erase existing opp, opp.evt.ratio, and vct entries in main db for
    # files about to be merged. Otherwise we'll get sqlite3 errors about
    # "UNIQUE constraint failed" if filtering is being rerun for some files.
    for (f in get.opp.files(src)) {
      .delete.opp.by.file(f, db=db.name)
    }
    for (f in get.vct.files(src)) {
      .delete.vct.by.file(f, db=db.name)
    }
    for (f in get.opp.evt.ratio.files(src)) {
      .delete.opp.evt.ratio.by.file(f, db=db.name)
    }

    # Now merge src db into main db
    merge.sql <- paste(sprintf("attach \"%s\" as incoming", src),
                       "BEGIN",
                       sprintf("insert into %s select * from incoming.%s",
                               opp.table.name, opp.table.name),
                       sprintf("insert into %s select * from incoming.%s",
                               vct.table.name, vct.table.name),
                       sprintf("insert into %s select * from incoming.%s",
                               opp.evt.ratio.table.name, opp.evt.ratio.table.name),
                       "COMMIT;", sep="; ")
    cmd <- sprintf("sqlite3 %s '%s'", target.db, merge.sql)
    status <- system(cmd)
    if (status > 0) {
      stop(paste("Db merge command '", cmd, "' failed with exit status ", status))
    }
    file.remove(src)
  }
}

# Create empty sqlite db for this project. If one already exists it will be
# overwritten. Also erase any numbered sqlite3 dbs used for parallel filtering.
#
# Args:
#   db.loc: directory containing sqlite3 database(s)
#   parts.only: only erase numbered databases (e.g. popcycle.db5) and leave
#     main db untouched
reset.db <- function(db.loc=db.location, parts.only=FALSE) {
  if (parts.only) {
    db.files <- list.files(db.loc, pattern="^popcycle\\.db[0-9]+$", full.names=TRUE)
  } else {
    db.files <- list.files(db.loc, pattern="^popcycle\\.db[0-9]*$", full.names=TRUE)
  }
  for (db in db.files) {
      file.remove(db)
  }
  # Create empty sqlite database
  if (! parts.only) {
    make.sqlite.db(paste(db.loc, "popcycle.db", sep="/"))
  }
}

# Ensure that there is an sfl.date index in sqlite3 db
#
# Args:
#   db: path to sqlite3 db file
ensure.sfl.date.index <- function(db=db.name) {
  system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS sflDateIndex ON sfl (date)'"))
}

# Ensure that there is are per channel indexes on opp in sqlite3 db
#
# Args:
#   db: path to sqlite3 db file
ensure.opp.channel.indexes <- function(db=db.name) {
  system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS oppFsc_smallIndex ON opp (fsc_small)'"))
  #system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS oppFsc_perpIndex ON opp (fsc_perp)'"))
  #system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS oppFsc_bigIndex ON opp (fsc_big)'"))
  system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS oppPeIndex ON opp (pe)'"))
  system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS oppChl_smallIndex ON opp (chl_small)'"))
  #system(paste0("sqlite3 ", db, " 'CREATE INDEX IF NOT EXISTS oppChl_bigIndex ON opp (chl_big)'"))
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
    stop(paste("wrong format for date.string parameter : ", date.string, "instead of ", "%Y-%m-%d %H:%M"))
  }
  return(date.ct)
}

# Convert a POSIXct date into a string suitable for comparisons in db date
# fields comparison.
POSIXct.to.db.date <- function(date.ct) {
  return(format(date.ct, "%Y-%m-%dT%H:%M:00"))
}
