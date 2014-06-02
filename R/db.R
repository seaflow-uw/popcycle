library(RSQLite)

opp_to_db_opp <- function(opp, cruise.name, file.name) {
  n <- dim(opp)[1]
  new_columns = cbind(cruise = rep(cruise.name, n), file = rep(file.name, n), particle = 1:n)
  return (cbind(new_columns, opp))
}

upload_opp <- function(db.opp, con, table.name = 'opp') {
  dbWriteTable(conn = con, name = table.name, value = db.opp, row.names=FALSE, append=TRUE)
}

vct_to_db_vct <- function(vct, cruise.name, file.name, method.name) {
  n <- dim(opp)[1]
  cruise = rep(cruise.name, n)
  file = rep(file.name, n)
  particle = 1:n)
  pop <- vct$pop
  method <- rep(method.name, n)
  
  return (cbind(cruise, file, particle, pop, method))
}

upload_opp <- function(db.vct, con, table.name = 'vct') {
  dbWriteTable(conn = con, name = table.name, value = db.vct, row.names=FALSE, append=TRUE)
}