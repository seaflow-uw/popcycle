context("Gating operations")
library(popcycle)

source("helper.R")

test_that("Classify all files, one gating ID", {
  x <- setUp()

  # Create working db
  file.copy(x$db.full.one, file.path(x$tmp.dir, "test.db"))
  db <- file.path(x$tmp.dir, "test.db")
  reset_vct_table(db)
  if (nrow(get_vct_table(db, sfl_join = FALSE)) > 0) {  # make sure we deleted it
    stop("vct table not properly deleted")
  }
  mie <- read_mie_csv("../testdata/calibrated-mie.de77773.2020-10-23.csv")
  gating_id <- "0e283d1c-ba99-4dca-99bb-35735eeafc48"
  classify_opp_files(db, x$opp.one.input.dir, NULL, x$vct.output.dir,
                     mie_table = mie, gating_id = gating_id)

  # Expected contents
  vct_expected <- list(
    arrow::read_parquet(
      file.path(x$vct.one.input.dir, "2014-07-04T00-00-00+00-00.1H.vct.parquet")
    ),
    arrow::read_parquet(
      file.path(x$vct.one.input.dir, "2014-07-04T01-00-00+00-00.1H.vct.parquet")
    )
  )
  vct_got <- list(
    arrow::read_parquet(
      file.path(x$vct.output.dir, "2014-07-04T00-00-00+00-00.1H.vct.parquet")
    ),
    arrow::read_parquet(
      file.path(x$vct.output.dir, "2014-07-04T01-00-00+00-00.1H.vct.parquet")
    )
  )
  expect_equal(vct_got[[1]], vct_expected[[1]])
  expect_equal(vct_got[[2]], vct_expected[[2]])

  vct_table_expected <- get_vct_table(x$db.full.one)
  vct_table_got <- get_vct_table(db)

  tearDown(x)
})


test_that("Classify all files, filter and gating plan with two filter and gating IDs", {
  x <- setUp()

  # Create working db
  file.copy(x$db.full.plan, file.path(x$tmp.dir, "test.db"))
  db <- file.path(x$tmp.dir, "test.db")
  reset_vct_table(db)
  if (nrow(get_vct_table(db, sfl_join = FALSE)) > 0) {  # make sure we deleted it
    stop("vct table not properly deleted")
  }
  mie <- read_mie_csv("../testdata/calibrated-mie.de77773.2020-10-23.csv")
  classify_opp_files(db, x$opp.plan.input.dir, NULL, x$vct.output.dir, mie_table = mie)

  # Expected contents
  vct_expected <- list(
    arrow::read_parquet(
      file.path(x$vct.plan.input.dir, "2014-07-04T00-00-00+00-00.1H.vct.parquet")
    ),
    arrow::read_parquet(
      file.path(x$vct.plan.input.dir, "2014-07-04T01-00-00+00-00.1H.vct.parquet")
    )
  )
  vct_got <- list(
    arrow::read_parquet(
      file.path(x$vct.output.dir, "2014-07-04T00-00-00+00-00.1H.vct.parquet")
    ),
    arrow::read_parquet(
      file.path(x$vct.output.dir, "2014-07-04T01-00-00+00-00.1H.vct.parquet")
    )
  )
  expect_equal(vct_got[[1]], vct_expected[[1]])
  expect_equal(vct_got[[2]], vct_expected[[2]])

  vct_table_expected <- get_vct_table(x$db.full.plan)
  vct_table_got <- get_vct_table(db)

  tearDown(x)
})
