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
