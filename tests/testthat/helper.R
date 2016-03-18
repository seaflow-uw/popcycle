# Common testthat helper funtions
library(uuid)

setUp <- function() {
  x <- list()
  x$tmp.dir <- file.path(tempdir(), UUIDgenerate())
  dir.create(x$tmp.dir)
  x$cruise <- "testcruise"
  x$evt.dir <- system.file("extdata/SeaFlow/datafiles/evt", package="popcycle")
  x$opp.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_opp"))
  x$vct.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_vct"))
  x$db <- file.path(x$tmp.dir, paste0(x$cruise, ".db"))
  make.popcycle.db(x$db)
  return(x)
}

tearDown <- function(x) {
  unlink(x$tmp.dir, recursive=T)
}
