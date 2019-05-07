context("Gating operations")
library(popcycle)

source("helper.R")

test_that("Classify one file", {
  x <- setUp()

  opp_file <- "2014_185/2014-07-04T00-00-02+00-00"
  vct_dir <- file.path(x$tmp.dir, "vct")
  vct_stats_expected <- get.vct.stats.by.file(x$db.full, opp_file)
  vct_expected_25 <- get.vct.by.file(x$vct.input.dir, opp_file, 2.5)
  vct_expected_50 <- get.vct.by.file(x$vct.input.dir, opp_file, 50)
  vct_expected_975 <- get.vct.by.file(x$vct.input.dir, opp_file, 97.5)

  reset.vct.stats.table(x$db.full)
  expect_equal(nrow(get.vct.table(x$db.full)), 0)  # make sure we deleted it
  classify.opp.files(x$db.full, x$opp.input.dir, c(opp_file), vct_dir)

  vct_25 <- get.vct.by.file(vct_dir, opp_file, 2.5)
  vct_50 <- get.vct.by.file(vct_dir, opp_file, 50)
  vct_975 <- get.vct.by.file(vct_dir, opp_file, 97.5)
  expect_equal(vct_25, vct_expected_25)
  expect_equal(vct_50, vct_expected_50)
  expect_equal(vct_975, vct_expected_975)

  vct_stats <- get.vct.stats.by.file(x$db.full, opp_file)
  expect_equal(vct_stats, vct_stats_expected)
  expect_equal(unique(vct_stats$file), "2014_185/2014-07-04T00-00-02+00-00")

  tearDown(x)
})
