context("Date and time manipulations")
library(popcycle)

source("helper.R")

test_that("Timestamp component of SeaFlow files are parsed correctly", {
  expected <- c(
      lubridate::ymd_hms("2020-07-23 14:04:05+00:00"),
      lubridate::ymd_hms("2020-07-25 10:03:01+00:00"),
      NA
  )
  test_input <- c(
    "2020-07-23T14-04-05+00-00",
    "2020-07-25T10-03-01+00-00",
    "foo"
  )
  got <- suppressWarnings(from_filename_date_str(test_input))
  expect_equal(got[1:2], expected[1:2])
  expect_true(is.na(got[3]))
})

test_that("Timestamp creation", {
  dates <- c(
    lubridate::ymd_hms("2020-07-23T14:04:05+00:00"),
    lubridate::ymd_hms("2022-07-23T14:04:05+00:00")
  )
  expect_equal(
    to_date_str(dates),
    c("2020-07-23T14:04:05+00:00", "2022-07-23T14:04:05+00:00")
  )
  expect_error(to_date_str("2020-07-23T14:04:05+00:00"))
})

test_that("Filename timestamp creation", {
  dates <- c(
    lubridate::ymd_hms("2020-07-23T14:04:05+00:00"),
    lubridate::ymd_hms("2022-07-23T14:04:05+00:00")
  )
  expect_equal(
    to_filename_date_str(dates),
    c("2020-07-23T14-04-05+00-00", "2022-07-23T14-04-05+00-00")
  )
})
