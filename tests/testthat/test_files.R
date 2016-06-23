library(popcycle)
source("helper.R")

context("EVT file discovery")

test_that("EVT files are located correctly", {
  x <- setUp()

  answer <- c("2014_185/2014-07-04T00-00-02+00-00",
              "2014_185/2014-07-04T00-03-02+00-00",
              "2014_185/2014-07-04T00-06-02+00-00",
              "2014_185/2014-07-04T00-09-02+00-00",
              "2014_185/2014-07-04T00-12-02+00-00"
            )
  expect_equal(get.evt.files(x$evt.input.dir), sort(answer))

  tearDown(x)
})

test_that("Latest EVT file found correctly", {
  x <- setUp()

  answer <- "2014_185/2014-07-04T00-12-02+00-00"
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
