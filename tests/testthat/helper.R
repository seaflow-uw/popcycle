# Common testthat helper funtions
library(uuid)

setUp <- function() {
  x <- list()
  x$tmp.dir <- file.path(tempdir(), UUIDgenerate())
  #print(x$tmp.dir)
  dir.create(x$tmp.dir)
  x$cruise <- "testcruise"
  x$serial <- "740"
  # Input EVT data for tests
  x$evt.input.dir <- file.path("..", "testdata", "evt")
  x$slope.file <- file.path("..", "testdata", "seaflow_filter_slopes.csv")
  x$sfl.file <- file.path(x$evt.input.dir, "2014_185", "2014-07-04T00-00-00+00-00.sfl")
  # Output OPP directory for filter tests
  x$opp.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_opp"))
  # Output VCT directory for gating tests
  x$vct.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_vct"))
  # Empty output DB file for DB tests
  x$db <- file.path(x$tmp.dir, paste0(x$cruise, ".db"))
  make.popcycle.db(x$db)

  # Below are prebuilt files for SQLite3 DB, OPP, and VCT data.
  # Copy dbs to tempdir before tests
  file.copy("../testdata/testcruise_full.db", x$tmp.dir, copy.mode=F)
  file.copy("../testdata/testcruise_bare.db", x$tmp.dir, copy.mode=F)
  # Database with fixed results. May not exactly represent current filter settings,
  # but it's here to insulate non-filtering tests from filtering changes.
  x$db.full <- file.path(x$tmp.dir, "testcruise_full.db")
  # Same database without opp or vct tables, but with sfl, filter, gating.
  # Ready for filtering and gating tests
  x$db.bare <- file.path(x$tmp.dir, "testcruise_bare.db")
  x$opp.input.dir <- "../testdata/opp"
  x$vct.input.dir <- "../testdata/vct"

  return(x)
}

tearDown <- function(x) {
  unlink(x$tmp.dir, recursive=T)
}
