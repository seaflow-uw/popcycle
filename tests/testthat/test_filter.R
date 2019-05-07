context("EVT filtering")
library(popcycle)

test_that("Filter EVT files", {
  x <- setUp()

  evt_files <- get.evt.files(x$evt.input.dir)
  expect_equal(length(evt_files), 9)

  # Should filter EVT files that are also in the SFL table (7 of 9 files)
  expect_warning(filter.evt.files(x$db.bare, x$evt.input.dir, evt_files, x$opp.dir))

  opp_table_expected <- get.opp.table(x$db.full)
  opp_table <- get.opp.table(x$db.bare)
  expect_equal(opp_table, opp_table_expected)

  # Make sure written OPP files have the same number of particles as those
  # recorded in database opp table
  # Test DB results first
  opp_files_expected <- c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00")
  opp_files <- get.opp.files(x$db.bare)
  expect_equal(opp_files, opp_files_expected)

  # This should be the intersection of EVT files and SFL files
  all_opp_files_expected <- c(
    "2014_185/2014-07-04T00-00-02+00-00",
    "2014_185/2014-07-04T00-03-02+00-00",
    "2014_185/2014-07-04T00-06-02+00-00",
    "2014_185/2014-07-04T00-09-02+00-00",
    "2014_185/2014-07-04T00-12-02+00-00",
    "2014_185/2014-07-04T00-15-02+00-00",
    "2014_185/2014-07-04T00-17-02+00-00"
  )
  all_opp_files <- get.opp.files(x$db.bare, all.files=TRUE)
  expect_equal(all_opp_files, all_opp_files_expected)

  # Then check that correct OPP files actually exist
  on_disk_files_expected <- c("2014-07-04T00-00-02+00-00.opp.gz", "2014-07-04T00-03-02+00-00.opp.gz")
  on_disk_files <- list.files(file.path(x$opp.dir, "2014_185"), pattern=".*\\.opp\\.gz")
  expect_equal(on_disk_files, on_disk_files_expected)

  for (opp_file in opp_files) {
    for (q in c(2.5, 50, 97.5)) {
      opp <- get.opp.by.file(x$opp.dir, opp_file, q)
      expect_equal(opp_table[opp_table$file == opp_file & opp_table$quantile == q, "opp_count"], nrow(opp))
      # Make sure it equals reference OPP file. This will detect if we start
      # writing bad opp files.
      expect_equal(opp, get.opp.by.file(x$opp.input.dir, opp_file, q))
    }
  }

  outlier_table_expected <- get.outlier.table(x$db.full)
  outlier_table <- get.outlier.table(x$db.bare)
  expect_equal(outlier_table, outlier_table_expected)
  expect_equal(outlier_table$file, all_opp_files_expected)
  expect_equal(outlier_table$flag, rep(0L, length(all_opp_files_expected)))

  tearDown(x)
})
