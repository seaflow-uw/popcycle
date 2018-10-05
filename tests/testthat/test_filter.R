context("EVT filtering")
library(popcycle)

test_that("Filter EVT files", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.input.dir)

  expect_warning(filter.evt.files(x$db.bare, x$evt.input.dir, evt.files, x$opp.dir))
  filter.params <- get.filter.params.latest(x$db.bare)
  opp.stats <- get.opp.table(x$db.bare)

  expect_equal(nrow(opp.stats), 15)  # 5 files, 3 quantiles each

  # Sort by file then quantile
  opp.stats <- opp.stats[order(opp.stats$file, opp.stats$quantile), ]

  expect_equal(opp.stats[, "filter_id"], rep(filter.params$id[1], 15))
  expect_equal(unique(opp.stats[, "file"]), evt.files)
  expect_equal(opp.stats[, "all_count"], c(rep(40000, 6), rep(0, 9)))
  expect_equal(opp.stats[, "evt_count"], c(rep(39928, 3), rep(39925, 3), rep(0, 9)))
  expect_equal(opp.stats[, "opp_count"], c(423, 107, 86, 492, 182, 147, rep(0, 9)))
  expect_equal(opp.stats[, "opp_evt_ratio"], c(0.010594069, 0.002679824, 0.002153877, 0.012323106, 0.004558547, 0.003681904, rep(0, 9)))
  expect_equal(opp.stats[, "quantile"], rep(c(2.5, 50, 97.5), 5))

  # Make sure written OPP files have the same number of particles as those
  # recorded in database opp table
  opp.files <- get.opp.files(x$db.bare)
  expect_equal(length(opp.files), 2)
  all.opp.files <- get.opp.files(x$db.bare, all.files=TRUE)
  expect_equal(length(all.opp.files), 5)
  i <- 1
  for (opp.file in opp.files) {
    for (q in c(2.5, 50, 97.5)) {
      opp <- get.opp.by.file(x$opp.dir, opp.file, q)
      expect_equal(opp.stats[opp.stats$file == opp.file & opp.stats$quantile == q, "opp_count"], nrow(opp))
    }
  }

  tearDown(x)
})
