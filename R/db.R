library(RSQLite)

opp.to.db.opp <- function(opp, cruise.name, file.name) {
  n <- dim(opp)[1]
  new.columns = cbind(cruise = rep(cruise.name, n), file = rep(file.name, n), particle = 1:n)
  return (cbind(new.columns, opp))
}

upload.opp <- function(db.opp) {
  con <- dbConnect(SQLite(), dbname = db.name)
  dbWriteTable(conn = con, name = opp.table.name, value = db.opp, row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

# these delete functions should only be called when re-running analyses
.delete.opp.by.file <- function(file.name) {
  sql <- paste0("DELETE FROM ", opp.table.name, " WHERE file == '", 
                file.name, "'")
  con <- dbConnect(SQLite(), dbname = db.name)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.vct.by.file <- function(file.name) {
  sql <- paste0("DELETE FROM ", vct.table.name, " WHERE file == '", 
                file.name, "'")
  con <- dbConnect(SQLite(), dbname = db.name)
  dbGetQuery(con, sql)
  dbDisconnect(con)
}

.delete.evt.count.by.file <- function(file.name) {
  sql <- paste0("DELETE FROM ", evt.count.table.name, " WHERE file == '", 
                file.name, "'")
  con <- dbConnect(SQLite(), dbname = db.name)
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

upload.vct <- function(db.vct) {
  con <- dbConnect(SQLite(), dbname = db.name)
  dbWriteTable(conn = con, name = vct.table.name, value = db.vct, row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

get.opp.by.file <- function(file.name) {
  sql <- paste0("SELECT * FROM ", opp.table.name, " WHERE file == '", 
                file.name, "' ORDER BY particle")
  con <- dbConnect(SQLite(), dbname = db.name)
  opp <- dbGetQuery(con, sql)
  dbDisconnect(con)
  # drop cruise, file, particle columns
  return (opp[,-c(1,2,3)])
}

get.vct.by.file <- function(file.name) {
  sql <- paste0("SELECT * FROM ", vct.table.name, " WHERE file == '", 
                file.name, "' ORDER BY particle")
  con <- dbConnect(SQLite(), dbname = db.name)
  vct <- dbGetQuery(con, sql)
  dbDisconnect(con)
  # drop cruise, file, particle, method columns
  return (vct[,-c(1,2,3,5)])
}

upload.evt.count <- function(evt, cruise.name, file.name) {
  con <- dbConnect(SQLite(), dbname = db.name)
  dbWriteTable(conn = con, name = evt.count.table.name, 
               value = data.frame(cruise = cruise.name, file = file.name, count = dim(evt)[1]),
               row.names=FALSE, append=TRUE)
  dbDisconnect(con)
}

get.opp.by.files <- function(...) {
  #TODO(hyrkas): implement
}

get.vct.by.files <- function(...) {
  #TODO(hyrkas): implement
}