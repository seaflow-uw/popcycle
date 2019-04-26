context("EVT file discovery")
library(popcycle)

source("helper.R")

test_that("EVT files are located correctly", {
  x <- setUp()

  answer <- c("2014_185/2014-07-04T00-00-02+00-00",
              "2014_185/2014-07-04T00-03-02+00-00",
              "2014_185/2014-07-04T00-06-02+00-00",
              "2014_185/2014-07-04T00-09-02+00-00",
              "2014_185/2014-07-04T00-12-02+00-00",
              "2014_185/2014-07-04T00-15-02+00-00",
              "2014_185/2014-07-04T00-17-02+00-00",
              "2014_185/2014-07-04T00-21-02+00-00",
              "2014_185/2014-07-04T00-27-02+00-00"
            )
  expect_equal(get.evt.files(x$evt.input.dir), sort(answer))

  tearDown(x)
})

test_that("Latest EVT file found correctly", {
  x <- setUp()

  answer <- "2014_185/2014-07-04T00-27-02+00-00"
  expect_equal(get.latest.evt(x$evt.input.dir), answer)

  tearDown(x)
})

test_that("Clean file paths", {
  old.files <- c(
    "2013_111/1.evt",
    "foo/2013_111/1.evt",
    "2013_111/1.evt.opp",
    "2013_111/1.evt.vct",
    "2013_111/1.evt.gz",
    "2013_111/1.evt.opp.gz",
    "2013_111/1.evt.vct.gz"
  )
  new.files <- c(
    "2013_111/2014-07-04T00-00-02+00-00",
    "foo/2013_111/2014-07-04T00-00-02+00-00",
    "2013_111/2014-07-04T00-00-02+00-00.opp",
    "2013_111/2014-07-04T00-00-02+00-00.vct",
    "2013_111/2014-07-04T00-00-02+00-00.gz",
    "2013_111/2014-07-04T00-00-02+00-00.opp.gz",
    "2013_111/2014-07-04T00-00-02+00-00.vct.gz"
  )

  old.files.clean <- unlist(lapply(old.files, clean.file.path))
  new.files.clean <- unlist(lapply(new.files, clean.file.path))

  expect_equal(old.files.clean, rep("2013_111/1.evt", length(old.files)))
  expect_equal(new.files.clean, rep("2013_111/2014-07-04T00-00-02+00-00", length(new.files)))
})

test_that("Read EVT files", {
  x <- setUp()

  # Good file, uncompressed
  df = readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00"))
  expect_equal(nrow(df), 40000)
  expect_known_hash(df, '77b5c5ecbb')
  # Good file, compressed
  df = readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00.gz"))
  expect_equal(nrow(df), 40000)
  expect_known_hash(df, 'd8d5667a31')
  # size 0 file
  expect_warning(readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-06-02+00-00")))
  # No particles, header == 0
  expect_warning(readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-09-02+00-00")))
  # bad 2 byte header, should be 4
  expect_warning(readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-12-02+00-00")))
  # more data than header reports
  expect_warning(readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-21-02+00-00")))
  # less data than header reports
  expect_warning(readSeaflow(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-27-02+00-00")))

  tearDown(x)
})

test_that("Filter file list", {
  files <- c(
    "tests/testcruise_evt/2014_185/2014-07-04T00-00-02+00-00",
    "tests/testcruise_evt/2014_185/2014-07-04T00-03-02+00-00.gz",
    "tests/testcruise_evt/2014_185/2014-07-04T00-06-02+00-00",
    "tests/testcruise_evt/2014_185/2014-07-04T00-09-02+00-00",
    "tests/testcruise_evt/2014_185/2014-07-04T00-12-02+00-00",
    "tests/testcruise_evt/2014_185/2014-07-04T00-15-02+00-00.gz",
    "tests/testcruise_evt/2014_185/2014-07-04T00-17-02+00-00.gz",
    "tests/testcruise_evt/2014_185/2014-07-04T00-21-02+00-00"
  )
  filter_list <- c(
    "2014_185/2014-07-04T00-00-02+00-00",
    "testcruise_evt/2014_185/2014-07-04T00-03-02+00-00.gz",
    "foo/2014_185/2014-07-04T00-06-02+00-00",
    "2014_185/2014-07-04T00-17-02+00-00.gz",
    "2014_185/2014-07-04T00-30-02+00-00.gz"
  )
  answer <- c(
    "tests/testcruise_evt/2014_185/2014-07-04T00-00-02+00-00",
    "tests/testcruise_evt/2014_185/2014-07-04T00-03-02+00-00.gz",
    "tests/testcruise_evt/2014_185/2014-07-04T00-06-02+00-00",
    "tests/testcruise_evt/2014_185/2014-07-04T00-17-02+00-00.gz"
  )
  result <- select_files_in(files, filter_list)
  expect_equal(answer, result)
})