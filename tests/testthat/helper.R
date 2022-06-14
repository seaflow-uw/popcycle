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
  x$evtv2.input.dir <- file.path("..", "testdata", "evt_v2")
  x$slope.file <- file.path("..", "testdata", "seaflow_filter_slopes.csv")
  x$sfl.file <- file.path(x$evt.input.dir, "2014_185", "2014-07-04T00-00-00+00-00.sfl")

  # Output OPP directory for filter tests
  x$opp.output.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_opp"))
  # Output VCT directory for gating tests
  x$vct.output.dir <- file.path(x$tmp.dir, paste0(x$cruise, "_vct"))
  # Empty output DB file for DB tests
  x$db <- file.path(x$tmp.dir, paste0(x$cruise, ".db"))
  make_popcycle_db(x$db)

  # Below are prebuilt files for SQLite3 DB, OPP, and VCT data.
  # Copy dbs to tempdir before tests
  file.copy("../testdata/testcruise_full_one_param.db", x$tmp.dir, copy.mode=F)
  file.copy("../testdata/testcruise_full_plan.db", x$tmp.dir, copy.mode=F)
  file.copy("../testdata/testcruise_bare.db", x$tmp.dir, copy.mode=F)
  # Database with fixed results. May not exactly represent current filter settings,
  # but it's here to insulate non-filtering tests from filtering changes.
  x$db.full.one <- file.path(x$tmp.dir, "testcruise_full_one_param.db")
  x$db.full.plan <- file.path(x$tmp.dir, "testcruise_full_plan.db")
  # Same database without opp or vct tables, but with sfl, filter, gating.
  # Ready for filtering and gating tests
  x$db.bare <- file.path(x$tmp.dir, "testcruise_bare.db")
  # Prepared gates
  x$gates1.file <- "../testdata/gates1.rds"
  x$gates2.file <- "../testdata/gates2.rds"
  # Pre-generated OPP data for one set of filter paramters matching latest
  # filter parameters
  x$opp.one.input.dir <- "../testdata/opp_one_param"
  # Pre-generated OPP data for two sets of filter parameters matching filter_plan
  # table
  x$opp.plan.input.dir <- "../testdata/opp_plan"
  x$vct.one.input.dir <- "../testdata/vct_one_param"
  x$vct.plan.input.dir <- "../testdata/vct_plan"
  # For get.vct* tests
  file.copy("../testdata/getvct/HOT303.db", x$tmp.dir, copy.mode=F)
  x$db.getvct <- file.path(x$tmp.dir, "HOT303.db")
  x$get.vct.input.dir <- "../testdata/getvct"

  # Below are prebuilt files for PSD SQLite3 DB and VCT data.
  x$psd.db <- "../testdata/psd/SCOPE_19.db"
  x$psd.vct.dir <- "../testdata/psd/SCOPE_19_vct"

  return(x)
}

tearDown <- function(x) {
  unlink(x$tmp.dir, recursive=T)
}
