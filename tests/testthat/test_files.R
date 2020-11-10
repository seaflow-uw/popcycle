context("SeaFlow file manipulations")
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

test_that("Read labview EVT files", {
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

test_that("Read OPP by file", {
  x <- setUp()

  df <- get.opp.by.file(x$db.full, x$opp.input.dir, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(nrow(df), 426)
  df <- get.opp.by.file(x$db.full, x$opp.input.dir, "2014_185/2014-07-04T00-00-02+00-00",
                        col_select=c("D1"))
  expect_equal(nrow(df), 426)
  expect_equal(names(df), c("date", "file_id", "D1"))

  tearDown(x)
})

test_that("Read OPP by date", {
  x <- setUp()

  df <- get.opp.by.date(x$db.full, x$opp.input.dir, "2014-07-04 00:00", "2014-07-04 00:02")
  expect_equal(nrow(df), 426)
  df <- get.opp.by.date(x$db.full, x$opp.input.dir, "2014-07-04 00:00", "2014-07-04 00:02",
                        col_select=c("D1"))
  expect_equal(names(df), c("date", "file_id", "D1"))
  expect_equal(nrow(df), 426)
  df <- get.opp.by.date(x$db.full, x$opp.input.dir, "2014-07-04 00:03", "2014-07-04 00:06")
  expect_equal(nrow(df), 495)
  df <- get.opp.by.date(x$db.full, x$opp.input.dir, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(nrow(df), 921)

  save.outliers(x$db.full, data.frame(file=c("2014_185/2014-07-04T00-00-02+00-00"), flag=1))
  df <- get.opp.by.date(x$db.full, x$opp.input.dir, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(nrow(df), 495)
  df <- get.opp.by.date(x$db.full, x$opp.input.dir, "2014-07-04 00:00", "2014-07-04 00:04",
                        outliers=FALSE)
  expect_equal(nrow(df), 921)

  tearDown(x)
})

test_that("Read single VCT by file name", {
  db <- "../testdata/getvct/HOT303.db"
  vct_dir <- "../testdata/getvct"
  vct <- get.vct.by.file(db, vct_dir, "2018_176/2018-06-25T20-03-48+00-00")
  expect_equal(as.vector(unique(vct$file_id)), "2018_176/2018-06-25T20-03-48+00-00")
  expect_equal(nrow(vct), 2601)

  # Column selection
  vct <- get.vct.by.file(db, vct_dir, "2018_176/2018-06-25T20-03-48+00-00", col_select=c("q50", "D1"))
  expect_equal(as.vector(unique(vct$file_id)), "2018_176/2018-06-25T20-03-48+00-00")
  expect_equal(nrow(vct), 2601)
  expect_equal(names(vct), c("date", "file_id", "q50", "D1"))
})

test_that("Read non-existent VCT by file name", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  vct <- get.vct.by.file(db, vct_dir, "2018_172/2018-06-25T20-03-48+00-00")
  expect_equal(nrow(vct), 0)
})

test_that("Read VCT by date range, one file ID returned", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  start_date <- "2018-06-25 20:00"
  end_date <- "2018-06-25 20:02"
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date)
  expect_equal(nrow(vct), 1792)
  expect_equal(as.character(unique(vct$file_id)), "2018_176/2018-06-25T20-00-48+00-00")
})

test_that("Read VCT by date range, three file IDs returned from two parquets", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  start_date <- "2018-06-25 20:00"
  end_date <- "2018-06-25 21:04"
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date)
  expect_equal(nrow(vct), 25126)
  expect_equal(
    as.character(unique(vct$file_id)),
    c(
      "2018_176/2018-06-25T20-00-48+00-00",
      "2018_176/2018-06-25T20-03-48+00-00",
      "2018_176/2018-06-25T21-03-52+00-00"
    )
  )
})

test_that("Read VCT by date range, no file ID returned", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  start_date <- "2018-06-25 21:10"
  end_date <- "2018-06-25 21:20"
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date)
  expect_equal(nrow(vct), 0)
})


test_that("Read VCT by date range, with outliers", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  # Set second file as outlier
  save.outliers(db, data.frame(file=c("2018_176/2018-06-25T20-03-48+00-00"), flag=1))

  start_date <- "2018-06-25 20:00"
  end_date <- "2018-06-25 21:04"
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date)
  expect_equal(nrow(vct), 22525)
  expect_equal(
    as.character(unique(vct$file_id)),
    c(
      "2018_176/2018-06-25T20-00-48+00-00",
      "2018_176/2018-06-25T21-03-52+00-00"
    )
  )

  # Without outlier filtering
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date, outliers=F)
  expect_equal(nrow(vct), 25126)
  expect_equal(
    as.character(unique(vct$file_id)),
    c(
      "2018_176/2018-06-25T20-00-48+00-00",
      "2018_176/2018-06-25T20-03-48+00-00",
      "2018_176/2018-06-25T21-03-52+00-00"
    )
  )
})

test_that("Read VCT by date range, col_select specified", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  start_date <- "2018-06-25 20:00"
  end_date <- "2018-06-25 20:02"
  # date and file_id should be added automatically as first two columns
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date, col_select=c(q50, fsc_small, diam_mid_q50))
  expect_equal(nrow(vct), 1792)
  expect_equal(as.character(unique(vct$file_id)), "2018_176/2018-06-25T20-00-48+00-00")
  expect_equal(names(vct), c("date", "file_id", "q50", "fsc_small", "diam_mid_q50"))
  expect_equal(sum(vct$q50), 1504)

  # But if we ask for data and file_id that should be fine too
  vct <- get.vct.by.date(db, vct_dir, start_date, end_date, col_select=c(date, file_id, q50, fsc_small, diam_mid_q50))
  expect_equal(nrow(vct), 1792)
  expect_equal(as.character(unique(vct$file_id)), "2018_176/2018-06-25T20-00-48+00-00")
  expect_equal(names(vct), c("date", "file_id", "q50", "fsc_small", "diam_mid_q50"))
  expect_equal(sum(vct$q50), 1504)
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

test_that("Convert time-windowed file paths to intervals", {
  window_df <- tibble::tibble(
    window_path=c(
      "path/component/2014-07-04T00-00-00+00-00.1H.opp.parquet",
      "path/component/2014-07-04T01-00-00+00-00.1H.vct.parquet",
      "2014-07-04T02-00-00+00-00.1H.opp.parquet",
      "2014-07-04T03-00-00+00-00.1H.vct.parquet",
      "abcdefT03-00-00+00-00.1H.vct.parquet",
      "2014-07-04T04-00-00+00-00.10H.opp.parquet",
      "2014-07-04T05-00-00+00-00.10H.vct.parquet"
    )
  )
  duration1 <- lubridate::hours(1) - lubridate::milliseconds(1)
  duration10 <- lubridate::hours(10) - lubridate::milliseconds(1)
  answer <- c(
    lubridate::as.interval(duration1 , start=lubridate::ymd_hms("2014-07-04 00:00:00+00:00")),
    lubridate::as.interval(duration1, start=lubridate::ymd_hms("2014-07-04 01:00:00+00:00")),
    lubridate::as.interval(duration1, start=lubridate::ymd_hms("2014-07-04 02:00:00+00:00")),
    lubridate::as.interval(duration1, start=lubridate::ymd_hms("2014-07-04 03:00:00+00:00")),
    lubridate::interval(start=NA, end=NA),
    lubridate::as.interval(duration10, start=lubridate::ymd_hms("2014-07-04 04:00:00+00:00")),
    lubridate::as.interval(duration10, start=lubridate::ymd_hms("2014-07-04 05:00:00+00:00"))
  )
  # File with bad timestamp will throw a warning
  result <- suppressWarnings(popcycle:::add_window_paths_intervals(window_df))
  expect_equal(result$window_path, window_df$window_path)
  expect_equal(result$interval, answer)
})

test_that("Match window paths to multiple dates", {
  df <- tibble::tibble(
    date=c(
      lubridate::ymd_hms("2020-07-23T23:00:00+00:00"),  # first window file
      lubridate::ymd_hms("2020-07-21T23:00:00+00:00"),  # no file, should be NA
      lubridate::ymd_hms("2020-07-24T23:00:00+00:00"),  # last window file
      lubridate::ymd_hms("2020-07-23T23:30:00+00:00")   # first window file
    ),
    file_id=c(
      "2020-07-23T23-00-00+00-00",
      "2020-07-21T23-00-00+00-00",
      "2020-07-24T23-00-00+00-00",
      "2020-07-23T23-30-00+00-00"
    )
  )
  window_paths <- c(
    "2020-07-23T23-00-00+00-00.1H.opp.parquet",
    "2020-07-23T07-00-00+00-00.1H.opp.parquet",
    "2020-07-24T23-00-00+00-00.1H.opp.parquet"
  )
  df_got <- popcycle:::add_window_paths(df, window_paths)
  expect_equal(df_got[, c("date", "file_id")], df[, c("date", "file_id")])
  intervals_expected <- c(
    "2020-07-23 23:00:00 UTC--2020-07-23 23:59:59 UTC",
    "NA--NA",
    "2020-07-24 23:00:00 UTC--2020-07-24 23:59:59 UTC",
    "2020-07-23 23:00:00 UTC--2020-07-23 23:59:59 UTC"
  )
  expect_equal(as.character(df_got$interval), intervals_expected)
  paths_expected <- c(
    "2020-07-23T23-00-00+00-00.1H.opp.parquet",
    NA,
    "2020-07-24T23-00-00+00-00.1H.opp.parquet",
    "2020-07-23T23-00-00+00-00.1H.opp.parquet"
  )
  expect_equal(as.character(df_got$window_path), paths_expected)
})

test_that("Match window paths to single matching date", {
  df <- tibble::tibble(
    date=c(lubridate::ymd_hms("2020-07-23T07:33:00+00:00")),
    file_id=c("2020-07-23T07-33-00+00-00")
  )
  window_paths <- c(
    "2020-07-23T23-00-00+00-00.1H.opp.parquet",
    "2020-07-23T07-00-00+00-00.1H.opp.parquet",
    "2020-07-24T23-00-00+00-00.1H.opp.parquet"
  )
  df_got <- popcycle:::add_window_paths(df, window_paths)
  expect_equal(df_got[, c("date", "file_id")], df[, c("date", "file_id")])
  intervals_expected <- c("2020-07-23 07:00:00 UTC--2020-07-23 07:59:59 UTC")
  expect_equal(as.character(df_got$interval), intervals_expected)
  paths_expected <- c("2020-07-23T07-00-00+00-00.1H.opp.parquet")
  expect_equal(as.character(df_got$window_path), paths_expected)
})

test_that("Match window paths to single unmatched date", {
  df <- tibble::tibble(
    date=c(lubridate::ymd_hms("2020-07-22T07:33:00+00:00")),
    file_id=c("2020-07-22T07-33-00+00-00")
  )
  window_paths <- c(
    "2020-07-23T23-00-00+00-00.1H.opp.parquet",
    "2020-07-23T07-00-00+00-00.1H.opp.parquet",
    "2020-07-24T23-00-00+00-00.1H.opp.parquet"
  )
  df_got <- popcycle:::add_window_paths(df, window_paths)
  expect_equal(df_got[, c("date", "file_id")], df[, c("date", "file_id")])
  intervals_expected <- c("NA--NA")
  expect_equal(as.character(df_got$interval), intervals_expected)
  expect_true(length(df_got$window_path) == 1 && is.na(df_got$window_path[1]))
})
