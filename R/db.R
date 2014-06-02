# TODO(hyrkas): decide if con should be invoked every upload operation or if there
# should be one connection parameter. right now we'll stick with invoking
# every time

# TODO(hyrkas): maybe add checks to make sure that db.name, opp.table.name and 
# vct.table.name are defined in the constants file?

library(RSQLite)

opp_to_db_opp <- function(opp, cruise.name, file.name) {
  n <- dim(opp)[1]
  new_columns = cbind(cruise = rep(cruise.name, n), file = rep(file.name, n), particle = 1:n)
  return (cbind(new_columns, opp))
}

upload_opp <- function(db.opp) {
  con <- dbConnect(SQLite(), dbname = db.name)
  dbWriteTable(conn = con, name = opp.table.name, value = db.opp, row.names=FALSE, append=TRUE)
}

vct_to_db_vct <- function(vct, cruise.name, file.name, method.name) {
  n <- dim(opp)[1]
  cruise = rep(cruise.name, n)
  file = rep(file.name, n)
  particle = 1:n
  pop <- vct$pop
  method <- rep(method.name, n)
  
  return (cbind(cruise, file, particle, pop, method))
}

upload_vct <- function(db.vct) {
  con <- dbConnect(SQLite(), dbname = db.name)
  dbWriteTable(conn = con, name = vct.table.name, value = db.vct, row.names=FALSE, append=TRUE)
}