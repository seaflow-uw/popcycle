context("EVT filtering")
library(popcycle)

test_that("Filter EVT files", {
  x <- setUp()

  evt_files <- get.evt.files(x$evt.input.dir)
  expect_equal(length(evt_files), 9)

  # These are EVT files in common with SFL files
  expected_files <- c(
    "2014_185/2014-07-04T00-00-02+00-00",
    "2014_185/2014-07-04T00-03-02+00-00",
    "2014_185/2014-07-04T00-06-02+00-00",
    "2014_185/2014-07-04T00-09-02+00-00",
    "2014_185/2014-07-04T00-12-02+00-00",
    "2014_185/2014-07-04T00-15-02+00-00",
    "2014_185/2014-07-04T00-17-02+00-00"
  )
  expected_files_n <- length(expected_files)

  # Should filter EVT files that are also in the SFL table (7 of 9 files)
  expect_warning(filter.evt.files(x$db.bare, x$evt.input.dir, evt_files, x$opp.dir))
  filter.params <- get.filter.params.latest(x$db.bare)

  # Should return all files we tried to filter
  opp.table <- get.opp.table(x$db.bare)
  expect_equal(nrow(opp.table), expected_files_n * 3)  # n files * 3 quantiles each
  # Sort by file then quantile
  opp.table <- opp.table[order(opp.table$file, opp.table$quantile), ]

  expect_equal(opp.table[, "filter_id"], rep(filter.params$id[1], expected_files_n * 3))
  expect_equal(unique(opp.table[, "file"]), expected_files)
  expect_equal(opp.table[, "all_count"], c(rep(40000, 6), rep(0, 9), rep(40000, 6)))
  expect_equal(opp.table[, "evt_count"], c(rep(39928, 3), rep(39925, 3), rep(0, 12), rep(39925, 3)))
  expect_equal(opp.table[, "opp_count"], c(423, 107, 86, 492, 182, 147, rep(0, 12), 0, 17, 19))
  expect_equal(opp.table[, "opp_evt_ratio"], c(0.010594069, 0.002679824, 0.002153877, 0.012323106, 0.004558547, 0.003681904, rep(0, 12), 0, 0.0004257984, 0.0004758923))
  expect_equal(opp.table[, "quantile"], rep(c(2.5, 50, 97.5), expected_files_n))
  
  outliers <- get.outlier.table(x$db.bare)
  expect_equal(outliers$file, expected_files)
  expect_equal(outliers$flag, rep(0L, expected_files_n))

  # Make sure written OPP files have the same number of particles as those
  # recorded in database opp table
  # Test DB results first
  opp.files <- get.opp.files(x$db.bare)
  expect_equal(opp.files, c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"))
  # This should only include files in SFL
  all.opp.files <- get.opp.files(x$db.bare, all.files=TRUE)
  expect_equal(all.opp.files, expected_files)

  # Then check that correct OPP files actually exist
  actual_files <- list.files(file.path(x$opp.dir, "2014_185"), pattern=".*\\.opp\\.gz")
  expect_equal(actual_files, c("2014-07-04T00-00-02+00-00.opp.gz", "2014-07-04T00-03-02+00-00.opp.gz"))

  for (opp.file in opp.files) {
    for (q in c(2.5, 50, 97.5)) {
      opp <- get.opp.by.file(x$opp.dir, opp.file, q)
      expect_equal(opp.table[opp.table$file == opp.file & opp.table$quantile == q, "opp_count"], nrow(opp))
      # Make sure it equals reference OPP file. This will detect if we start
      # writing bad opp files.
      expect_equal(opp, get.opp.by.file(x$opp.input.dir, opp.file, q))
    }
  }

  tearDown(x)
})
