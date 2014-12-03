library(RSQLite)

opp.to.db.opp <- function(opp, cruise.name, file.name) {
  n <- dim(opp)[1]
  new.columns = cbind(cruise = rep(cruise.name, n), file = rep(file.name, n), particle = 1:n)
  return (cbind(new.columns, opp))
}

upload.opp <- function(db.opp, db = db.name) {
  con <- dbConnect(SQLite(), dbname = db)
  dbWriteTable(conn = con, name = opp.table.name, value = db.opp, row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

# these delete functions should only be called when re-running analyses
.delete.opp.by.file <- function(file.name, db = db.name) {
  sql <- paste0("DELETE FROM ", opp.table.name, " WHERE file == '", 
                file.name, "'")
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.vct.by.file <- function(file.name, db = db.name) {
  sql <- paste0("DELETE FROM ", vct.table.name, " WHERE file == '", 
                file.name, "'")
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.opp.evt.ratio.by.file <- function(file.name, db = db.name) {
  sql <- paste0("DELETE FROM ", opp.evt.ratio.table.name, " WHERE file == '", 
                file.name, "'")
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.sfl <- function(db = db.name) {
  sql <- paste0("DELETE FROM ", sfl.table.name)
  con <- dbConnect(SQLite(), dbname = db)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

vct.to.db.vct <- function(vct, cruise.name, file.name, method.name) {
  n <- length(vct)
  cruise = rep(cruise.name, n)
  file = rep(file.name, n)
  particle = 1:n
  pop <- vct
  method <- rep(method.name, n)
  
  return (data.frame(cruise = cruise, file = file, particle = particle, pop = pop, method = method))
}

upload.vct <- function(db.vct, db = db.name) {
  con <- dbConnect(SQLite(), dbname = db)
  dbWriteTable(conn = con, name = vct.table.name, value = db.vct, row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

get.opp.by.file <- function(file.name, db = db.name) {
  sql <- paste0("SELECT * FROM ", opp.table.name, " WHERE file == '", 
                file.name, "' ORDER BY particle")
  con <- dbConnect(SQLite(), dbname = db)
  opp <- dbGetQuery(con, sql)
  dbDisconnect(con)
  # drop cruise, file, particle columns
  return (opp[,-c(1,2,3)])
}

get.vct.by.file <- function(file.name, db = db.name) {
  sql <- paste0("SELECT * FROM ", vct.table.name, " WHERE file == '", 
                file.name, "' ORDER BY particle")
  con <- dbConnect(SQLite(), dbname = db)
  vct <- dbGetQuery(con, sql)
  dbDisconnect(con)
  # drop cruise, file, particle, method columns
  return (vct[,-c(1,2,3,5)])
}

upload.opp.evt.ratio <- function(opp.evt.ratio, cruise.name, file.name, db = db.name) {
  con <- dbConnect(SQLite(), dbname = db)
  dbWriteTable(conn = con, name = opp.evt.ratio.table.name, 
               value = data.frame(cruise = cruise.name, file = file.name, ratio = opp.evt.ratio),
               row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

# Return a vector of distinct file values in opp table
#
# Args:
#   db = sqlite3 db
get.opp.list <- function(db = db.name) {
  sql <- paste0("SELECT DISTINCT file from ", opp.table.name)
  con <- dbConnect(SQLite(), dbname = db)
  files <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(files$file)
}

# Return a vector of distinct file values in vct.table.name
#
# Args:
#   db = sqlite3 db
get.vct.files <- function(db = db.name) {
  sql <- paste0("SELECT DISTINCT file from ", vct.table.name)
  con <- dbConnect(SQLite(), dbname = db)
  files <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(files$file)
}

# Return a vector of distinct file values in opp.evt.ratio.table.name
#
# Args:
#   db = sqlite3 db
get.opp.evt.ratio.files <- function(db = db.name) {
  sql <- paste0("SELECT DISTINCT file from ", opp.evt.ratio.table.name)
  con <- dbConnect(SQLite(), dbname = db)
  files <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return(files$file)
}

# Return a list of EVT files for which there is no OPP data in the database
#
# Args:
#   evt.list = list of EVT file paths, e.g. get.evt.list(evt.location)
#   db = sqlite3 db
get.empty.evt.files <- function(evt.list, db = db.name) {
  opp.files <- get.opp.list(db)
  return(setdiff(evt.list, opp.files))
}

get.stat.table <- function(db = db.name) {
  sql <- paste('SELECT * FROM ', stats.table.name)
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


upload.cytdiv <- function(indices, cruise.name, file.name, db = db.name) {
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
            sfl.file == cytdiv.file;"

  con <- dbConnect(SQLite(), dbname = db)
  cytdiv <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return (cytdiv)
}


get.sfl.table <- function(db = db.name) {
  sql <- paste('SELECT * FROM ', sfl.table.name)
  con <- dbConnect(SQLite(), dbname = db)
  sfl <- dbGetQuery(con, sql)
  dbDisconnect(con)
  return (sfl)
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

# Merge opp and opp.evt.ratio tables from multiple sqlite dbs into target.db.
# Erase files in src.dbs once merged.
# 
# Args:
#   src.dbs = paths of sqlite3 dbs to merge into target.db
#   target.db = path of sqlite3 db to be merged into
merge.dbs <- function(src.dbs, target.db=db.name) {
  for (src in src.dbs) {
    # First erase existing opp and opp.evt.ratio entries in main db for files
    # about to be merged.  Otherwise we'll get sqlite3 errors about 
    # "UNIQUE constraint failed" if filtering is being rerun for some files.
    for (f in get.opp.list(src)) {
      .delete.opp.by.file(f, db=db.name)
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

# Create empty sqlite db for this project.  If one already exists it will be
# overwritten.  Also erase any numbered sqlite3 dbs used for parallel filtering.
#
# Args:
#   db.loc = directory containing sqlite3 database(s)
#   parts.only = only erase numbered databases (e.g. popcycle.db5) and leave
#                main db untouched
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
