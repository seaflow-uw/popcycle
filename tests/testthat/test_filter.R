context("EVT filtering")
library(popcycle)

test_that("Filter EVT files", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.input.dir)

  expect_warning(filter.evt.files(x$db.bare, x$evt.input.dir, evt.files, x$opp.dir))
  filter.params <- get.filter.params.latest(x$db.bare)
  opp.stats <- get.opp.table(x$db.bare)

  expect_equal(nrow(opp.stats), 21)  # 7 files, 3 quantiles each

  # Sort by file then quantile
  opp.stats <- opp.stats[order(opp.stats$file, opp.stats$quantile), ]

  expect_equal(opp.stats[, "filter_id"], rep(filter.params$id[1], 21))
  expect_equal(unique(opp.stats[, "file"]), evt.files)
  expect_equal(opp.stats[, "all_count"], c(rep(40000, 6), rep(0, 9), rep(40000, 6)))
  expect_equal(opp.stats[, "evt_count"], c(rep(39928, 3), rep(39925, 3), rep(0, 12), rep(39925, 3)))
  expect_equal(opp.stats[, "opp_count"], c(423, 107, 86, 492, 182, 147, rep(0, 12), 0, 17, 19))
  expect_equal(opp.stats[, "opp_evt_ratio"], c(0.010594069, 0.002679824, 0.002153877, 0.012323106, 0.004558547, 0.003681904, rep(0, 12), 0, 0.0004257984, 0.0004758923))
  expect_equal(opp.stats[, "quantile"], rep(c(2.5, 50, 97.5), 7))

  # Make sure written OPP files have the same number of particles as those
  # recorded in database opp table
  # Test DB results first
  opp.files <- get.opp.files(x$db.bare)
  expect_equal(length(opp.files), 2)
  all.opp.files <- get.opp.files(x$db.bare, all.files=TRUE)
  expect_equal(length(all.opp.files), 7)

  # Then check how many OPP files actually exist
  actual_files <- list.files(file.path(x$opp.dir, "2014_185"), pattern=".*\\.opp\\.gz")
  expect_equal(length(actual_files), 2)

  for (opp.file in opp.files) {
    for (q in c(2.5, 50, 97.5)) {
      opp <- get.opp.by.file(x$opp.dir, opp.file, q)
      expect_equal(opp.stats[opp.stats$file == opp.file & opp.stats$quantile == q, "opp_count"], nrow(opp))
    }
  }

  tearDown(x)
})
