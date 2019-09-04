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

  channels <- c("D1", "D2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big")

  # Good file, uncompressed
  df <- get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(nrow(df), 40000)
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00.notransform.rds"))
  expect_equal(df, df_expected)
  
  df <- get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00", transform=T)
  expect_equal(nrow(df), 40000)
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00.transform.rds"))
  expect_equal(df, df_expected)

  # Good file, compressed
  df <- get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(nrow(df), 40000)
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00.notransform.rds"))
  expect_equal(df, df_expected)

  df <- get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00", transform=T)
  expect_equal(nrow(df), 40000)
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00.transform.rds"))
  expect_equal(df, df_expected)

  # Two EVT files at once
  df <- get.evt.by.file(x$evt.input.dir, c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"))
  expect_equal(nrow(df), 80000)

  # size 0 file
  expect_warning(get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-06-02+00-00"))

  # No particles, header == 0
  expect_warning(get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-09-02+00-00"))

  # bad 2 byte header, should be 4
  expect_warning(get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-12-02+00-00"))

  # more data than header reports
  expect_warning(get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-21-02+00-00"))

  # less data than header reports
  expect_warning(get.evt.by.file(x$evt.input.dir, "2014_185/2014-07-04T00-27-02+00-00"))

  tearDown(x)
})

test_that("Read OPP files", {
  x <- setUp()

  channels <- c("D1", "D2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big")

  # One quantile, transformed
  df <- get.opp.by.file(x$opp.input.dir, "2014_185/2014-07-04T00-00-02+00-00", quantile=50)
  expect_equal(ncol(df), 10)
  expect_equal(nrow(df), 107)
  expect_known_hash(df, 'a84c9ae9b1')
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed

  # One quantile, not transformed
  df <- get.opp.by.file(x$opp.input.dir, "2014_185/2014-07-04T00-00-02+00-00", quantile=50, transform=F)
  expect_equal(ncol(df), 10)
  expect_equal(nrow(df), 107)
  expect_known_hash(df, '1fd4e451b3')
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed

  # All quantiles, transformed
  df <- get.opp.by.file(x$opp.input.dir, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(ncol(df), 13)
  expect_equal(nrow(df), 426)
  expect_known_hash(df, 'cbc06fc003')
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed

  # All quantiles, not transformed
  df <- get.opp.by.file(x$opp.input.dir, "2014_185/2014-07-04T00-00-02+00-00", transform=F)
  expect_equal(ncol(df), 13)
  expect_equal(nrow(df), 426)
  expect_known_hash(df, '7be2bda6ea')
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed

  # Two OPP files at once
  df <- get.opp.by.file(x$opp.input.dir, c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"), quantile=50)
  expect_equal(nrow(df), 285)

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
  expect_equal(result, answer)
})