context("SeaFlow file manipulations")
library(popcycle)
library(dplyr)

source("helper.R")

test_that("EVT files are located correctly", {
  x <- setUp()

  paths <- c(
    "../testdata/evt/2014_185/2014-07-04T00-00-02+00-00",
    "../testdata/evt/2014_185/2014-07-04T00-03-02+00-00.gz",
    "../testdata/evt/2014_185/2014-07-04T00-06-02+00-00.gz",
    "../testdata/evt/2014_185/2014-07-04T00-09-02+00-00",
    "../testdata/evt/2014_185/2014-07-04T00-12-02+00-00",
    "../testdata/evt/2014_185/2014-07-04T01-15-02+00-00.gz",
    "../testdata/evt/2014_185/2014-07-04T01-17-02+00-00.gz",
    "../testdata/evt/2014_185/2014-07-04T01-21-02+00-00",  # not in sfl table
    "../testdata/evt/2014_185/2014-07-04T01-27-02+00-00",  # not in sfl table
    "../testdata/evt/2014_185/2014-07-04T01-30-02+00-00"
  )

  file_ids <- c(
    "2014_185/2014-07-04T00-00-02+00-00",
    "2014_185/2014-07-04T00-03-02+00-00",
    "2014_185/2014-07-04T00-06-02+00-00",
    "2014_185/2014-07-04T00-09-02+00-00",
    "2014_185/2014-07-04T00-12-02+00-00",
    "2014_185/2014-07-04T01-15-02+00-00",
    "2014_185/2014-07-04T01-17-02+00-00",
    "2014_185/2014-07-04T01-21-02+00-00",  # not in sfl table
    "2014_185/2014-07-04T01-27-02+00-00",  # not in sfl table
    "2014_185/2014-07-04T01-30-02+00-00"
  )

  dates <- c(
    "2014-07-04T00:00:02+00:00",
    "2014-07-04T00:03:02+00:00",
    "2014-07-04T00:06:02+00:00",
    "2014-07-04T00:09:02+00:00",
    "2014-07-04T00:12:02+00:00",
    "2014-07-04T01:15:02+00:00",
    "2014-07-04T01:17:02+00:00",
    NA,
    NA,
    "2014-07-04T01:30:02+00:00"
  )

  answer <- tibble::tibble(
    date = lubridate::ymd_hms(dates),
    path = paths,
    file_id = file_ids
  )

  expect_equal(get_evt_files(x$evt.input.dir), answer[, c("path", "file_id")])
  answer <- answer %>% filter(!is.na(date))
  expect_equal(get_evt_files(x$evt.input.dir, db = x$db.bare), answer)

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

  old.files.clean <- clean_file_path(old.files)
  new.files.clean <- clean_file_path(new.files)

  expect_equal(old.files.clean, rep("2013_111/1.evt", length(old.files)))
  expect_equal(new.files.clean, rep("2013_111/2014-07-04T00-00-02+00-00", length(new.files)))
})

test_that("Read labview EVT files", {
  x <- setUp()

  evt_files <- get_evt_files(x$evt.input.dir)

  channels <- c("D1", "D2", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small", "chl_big")

  # Good file, uncompressed
  df <- get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(nrow(df), 40000)
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00.notransform.rds"))
  expect_equal(df, df_expected)

  df <- get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00", transform=T)
  expect_equal(nrow(df), 40000)
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-00-02+00-00.transform.rds"))
  expect_equal(df, df_expected)

  # Good file, compressed
  df <- get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(nrow(df), 40000)
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00.notransform.rds"))
  expect_equal(df, df_expected)

  df <- get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00", transform=T)
  expect_equal(nrow(df), 40000)
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed
  df_expected <- readRDS(file.path(x$evt.input.dir, "2014_185/2014-07-04T00-03-02+00-00.transform.rds"))
  expect_equal(df, df_expected)

  # Two EVT files at once
  df <- get_evt_by_file(x$evt.input.dir, c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"))
  expect_equal(nrow(df), 80000)

  # size 0 file
  expect_warning(get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-06-02+00-00"))

  # No particles, header == 0
  expect_warning(get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-09-02+00-00"))

  # bad 2 byte header, should be 4
  expect_warning(get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T00-12-02+00-00"))

  # more data than header reports
  expect_warning(get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T01-21-02+00-00"))

  # less data than header reports
  expect_warning(get_evt_by_file(x$evt.input.dir, "2014_185/2014-07-04T01-27-02+00-00"))

  tearDown(x)
})


test_that("Read labview EVT v2 files", {
  x <- setUp()

  channels <- c("pulse_width", "chl_small", "D1", "D2", "fsc_small", "pe", "evt_rate")
  # The expected RDS file dataframes are v1 EVT dataframes, so we need to subset
  # down to common columns to compare to these v2 EVT dataframes.
  comp_channels <- channels[2:6]

  # Good file, uncompressed
  df <- get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(nrow(df), 40000)
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed
  df_expected <- readRDS(file.path(x$evtv2.input.dir, "2014_185/2014-07-04T00-00-02+00-00.notransform.rds"))
  expect_equal(df[comp_channels], df_expected[comp_channels])

  df <- get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-00-02+00-00", transform=T)
  expect_equal(nrow(df), 40000)
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed
  df_expected <- readRDS(file.path(x$evtv2.input.dir, "2014_185/2014-07-04T00-00-02+00-00.transform.rds"))
  expect_equal(df[comp_channels], df_expected[comp_channels])

  # Good file, compressed
  df <- get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(nrow(df), 40000)
  expect_true(any(max(df[, channels]) > 10^3.5))  # not transformed
  df_expected <- readRDS(file.path(x$evtv2.input.dir, "2014_185/2014-07-04T00-03-02+00-00.notransform.rds"))
  expect_equal(df[comp_channels], df_expected[comp_channels])

  df <- get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-03-02+00-00", transform=T)
  expect_equal(nrow(df), 40000)
  expect_false(any(max(df[, channels]) > 10^3.5))  # transformed
  df_expected <- readRDS(file.path(x$evtv2.input.dir, "2014_185/2014-07-04T00-03-02+00-00.transform.rds"))
  expect_equal(df[comp_channels], df_expected[comp_channels])

  # Two EVT files at once
  df <- get_evt_by_file(x$evtv2.input.dir, c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"))
  expect_equal(nrow(df), 80000)

  # size 0 file
  expect_warning(get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-06-02+00-00"))

  # No particles, header == 0
  expect_warning(get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-09-02+00-00"))

  # bad 2 byte header, should be 4
  expect_warning(get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T00-12-02+00-00"))

  # more data than header reports
  expect_warning(get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T01-21-02+00-00"))

  # less data than header reports
  expect_warning(get_evt_by_file(x$evtv2.input.dir, "2014_185/2014-07-04T01-27-02+00-00"))

  tearDown(x)
})

test_that("Read OPP by file", {
  x <- setUp()

  df <- get_opp_by_file(x$db.full.one, x$opp.one.input.dir, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(nrow(df), 426)
  df <- get_opp_by_file(x$db.full.one, x$opp.one.input.dir, "2014_185/2014-07-04T00-00-02+00-00",
                        col_select=c("D1"))
  expect_equal(nrow(df), 426)
  expect_equal(names(df), c("date", "file_id", "D1"))

  tearDown(x)
})

test_that("Read OPP by date", {
  x <- setUp()
  d <- lubridate::ymd_hm

  df <- get_opp_by_date(x$db.full.one, x$opp.one.input.dir, d("2014-07-04 00:00"), d("2014-07-04 00:02"))
  expect_equal(nrow(df), 426)
  df <- get_opp_by_date(x$db.full.one, x$opp.one.input.dir, d("2014-07-04 00:00"), d("2014-07-04 00:02"),
                        col_select=c("D1"))
  expect_equal(names(df), c("date", "file_id", "D1"))
  expect_equal(nrow(df), 426)
  df <- get_opp_by_date(x$db.full.one, x$opp.one.input.dir, d("2014-07-04 00:03"), d("2014-07-04 00:06"))
  expect_equal(nrow(df), 495)
  df <- get_opp_by_date(x$db.full.one, x$opp.one.input.dir, d("2014-07-04 00:00"), d("2014-07-04 00:04"))
  expect_equal(nrow(df), 921)

  save_outliers(x$db.full.one, data.frame(file_id=c("2014_185/2014-07-04T00-00-02+00-00"), flag=1))
  df <- get_opp_by_date(x$db.full.one, x$opp.one.input.dir, d("2014-07-04 00:00"), d("2014-07-04 00:04"))
  expect_equal(nrow(df), 495)
  df <- get_opp_by_date(x$db.full.one, x$opp.one.input.dir, d("2014-07-04 00:00"), d("2014-07-04 00:04"),
                        outliers=FALSE)
  expect_equal(nrow(df), 921)

  tearDown(x)
})

test_that("Read single VCT by file name", {
  db <- "../testdata/getvct/HOT303.db"
  vct_dir <- "../testdata/getvct"
  vct <- get_vct_by_file(db, vct_dir, "2018_176/2018-06-25T20-03-48+00-00")
  expect_equal(as.vector(unique(vct$file_id)), "2018_176/2018-06-25T20-03-48+00-00")
  expect_equal(nrow(vct), 2601)

  # Column selection
  vct <- get_vct_by_file(db, vct_dir, "2018_176/2018-06-25T20-03-48+00-00", col_select=c("q50", "D1"))
  expect_equal(as.vector(unique(vct$file_id)), "2018_176/2018-06-25T20-03-48+00-00")
  expect_equal(nrow(vct), 2601)
  expect_equal(names(vct), c("date", "file_id", "q50", "D1"))
})

test_that("Read non-existent VCT by file name", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  vct <- expect_warning(get_vct_by_file(db, vct_dir, "2018_172/2018-06-25T20-03-48+00-00"))
  expect_equal(nrow(vct), 0)
})

test_that("Read VCT by date range, one file ID returned", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  start_date <- lubridate::ymd_hms("2018-06-25 20:00:00")
  end_date <- lubridate::ymd_hms("2018-06-25 20:02:00")
  vct <- get_vct_by_date(db, vct_dir, start_date, end_date)
  expect_equal(nrow(vct), 1792)
  expect_equal(as.character(unique(vct$file_id)), "2018_176/2018-06-25T20-00-48+00-00")
})

test_that("Read VCT by date range, three file IDs returned from two parquets", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  start_date <- lubridate::ymd_hms("2018-06-25 20:00:00")
  end_date <- lubridate::ymd_hms("2018-06-25 21:04:00")
  vct <- get_vct_by_date(db, vct_dir, start_date, end_date)
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

  start_date <- lubridate::ymd_hms("2018-06-25 21:10:00")
  end_date <- lubridate::ymd_hms("2018-06-25 21:20:00")
  vct <- expect_warning(get_vct_by_date(db, vct_dir, start_date, end_date))
  expect_equal(nrow(vct), 0)
})


test_that("Read VCT by date range, with outliers", {
  x <- setUp()
  db <- x$db.getvct
  vct_dir <- x$get.vct.input.dir

  # Set second file as outlier
  save_outliers(db, data.frame(file_id=c("2018_176/2018-06-25T20-03-48+00-00"), flag=1))

  start_date <- lubridate::ymd_hms("2018-06-25 20:00:00")
  end_date <- lubridate::ymd_hms("2018-06-25 21:04:00")
  vct <- get_vct_by_date(db, vct_dir, start_date, end_date)
  expect_equal(nrow(vct), 22525)
  expect_equal(
    as.character(unique(vct$file_id)),
    c(
      "2018_176/2018-06-25T20-00-48+00-00",
      "2018_176/2018-06-25T21-03-52+00-00"
    )
  )

  # Without outlier filtering
  vct <- get_vct_by_date(db, vct_dir, start_date, end_date, outliers=F)
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

  start_date <- lubridate::ymd_hms("2018-06-25 20:00:00")
  end_date <- lubridate::ymd_hms("2018-06-25 20:02:00")
  # date and file_id should be added automatically as first two columns
  vct <- get_vct_by_date(db, vct_dir, start_date, end_date, col_select=c("q50", "fsc_small", "diam_mid_q50"))
  expect_equal(nrow(vct), 1792)
  expect_equal(as.character(unique(vct$file_id)), "2018_176/2018-06-25T20-00-48+00-00")
  expect_equal(names(vct), c("date", "file_id", "q50", "fsc_small", "diam_mid_q50"))
  expect_equal(sum(vct$q50), 1504)

  # But if we ask for data and file_id that should be fine too
  vct <- get_vct_by_date(db, vct_dir, start_date, end_date, col_select=c("date", "file_id", "q50", "fsc_small", "diam_mid_q50"))
  expect_equal(nrow(vct), 1792)
  expect_equal(as.character(unique(vct$file_id)), "2018_176/2018-06-25T20-00-48+00-00")
  expect_equal(names(vct), c("date", "file_id", "q50", "fsc_small", "diam_mid_q50"))
  expect_equal(sum(vct$q50), 1504)
})

test_that("Convert dates to hours and hourly intervals", {
  df <- tibble::tibble(
    date = c(
      lubridate::ymd_hms("2014-07-04T02:00:00+00:00"),
      lubridate::ymd_hms("2014-07-04T02:20:20+00:00")
    )
  )
  expected_hours <- c(
    lubridate::ymd_hms("2014-07-04T02:00:00+00:00"),
    lubridate::ymd_hms("2014-07-04T02:00:00+00:00")
  )
  expected_intervals <- c(
    lubridate::interval(
      lubridate::ymd_hms("2014-07-04T02:00:00+00:00"),
      lubridate::ymd_hms("2014-07-04T02:59:59.999+00:00")
    ),
    lubridate::interval(
      lubridate::ymd_hms("2014-07-04T02:00:00+00:00"),
      lubridate::ymd_hms("2014-07-04T02:59:59.999+00:00")
    )
  )
  result <- popcycle:::add_hours(df)

  expect_equal(result$hour, expected_hours)
  expect_equal(result$interval, expected_intervals)
})
