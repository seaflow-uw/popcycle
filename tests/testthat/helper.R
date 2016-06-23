# Common testthat helper funtions
library(uuid)

setUp <- function() {
  x <- list()
  x$tmp.dir <- file.path(tempdir(), UUIDgenerate())
  dir.create(x$tmp.dir)
  x$cruise <- "testcruise"
  # Input EVT data for tests
  x$evt.input.dir <- "../testdata/evt"
  # Output OPP directory for filter tests
  x$opp.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_opp"))
  # Output VCT directory for gating tests
  x$vct.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_vct"))
  # Output DB file for DB tests
  x$db <- file.path(x$tmp.dir, paste0(x$cruise, ".db"))
  # Below are prebuilt files for SQLite3 DB, OPP, and VCT data. These files are
  # for tests which do not explicitly test filter or gating results and should
  # not need to be updated when the filter or gating methods change.
  x$db.input <- "../testdata/testcruise.db"
  x$opp.input.dir <- "../testdata/opp"
  x$vct.input.dir <- "../testdata/vct"
  make.popcycle.db(x$db)
  return(x)
}

tearDown <- function(x) {
  unlink(x$tmp.dir, recursive=T)
}
