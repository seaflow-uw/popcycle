context("Gating operations")
library(popcycle)

source("helper.R")

test_that("Classify one file", {
  x <- setUp()

  opp_file <- "2014_185/2014-07-04T00-00-02+00-00"
  vct_dir <- file.path(x$tmp.dir, "vct")
  reset.vct.stats.table(x$db.full)
  expect_equal(nrow(get.vct.table(x$db.full)), 0)  # make sure we deleted it
  classify.opp.files(x$db.full, x$opp.input.dir, c(opp_file), vct_dir)
  vct <- get.vct.by.file(vct_dir, opp_file, 50)

  # Test some basic values from file
  expect_equal(
    mean(vct[vct$pop == "prochloro", "diam_lwr"]),
    1.063332310956445
  )
  expect_equal(nrow(vct), 107)

  # Test some basic values from database
  vct_stats <- get.vct.stats.by.file(x$db.full, opp_file)
  expect_equal(unique(vct_stats$file), "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(
    vct_stats[vct_stats$quantile == 50 & vct_stats$pop == "prochloro", "diam_lwr"],
    1.063332310956445
  )
  expect_equal(
    sum(vct_stats[vct_stats$quantile == 50, "count"]),
    107
  )
  expect_equal(
    vct_stats[vct_stats$pop == "prochloro" & vct_stats$quantile == 50, ]$count,
    72
  )

  tearDown(x)
})
