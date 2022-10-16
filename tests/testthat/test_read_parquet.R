context("SeaFlow parquet file reading convenience functions")
library(popcycle)
library(dplyr)

test_that("read_parquet_one_quantile, error when bad refractive index in refracs", {
  refracs <- data.frame(a = "lwr", b = "nnn")
  expect_error(read_parquet_one_quantile("x", 2.5, c("pe"), refracs = refracs))
})

test_that("read_parquet_one_quantile, error when too many rows in refracs", {
  refracs <- data.frame(
    a = c("lwr", "mid"),
    b = c("upr", "lwr")
  )
  expect_error(read_parquet_one_quantile("x",  2.5, c("pe"), refracs = refracs))
})

test_that("read_parquet_one_quantile, OPP, full file retrieval", {
  df <- make_small_vct() %>%
    select(-(ends_with("_q2.5") | ends_with("_q50") | ends_with("_q97.5")))
  want <- df %>%
    filter(q50) %>%
    select(date, D1, D2, fsc_small)
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  got <- read_parquet_one_quantile(path, 50)
  expect_equal(got, want)
})

test_that("read_parquet_one_quantile, OPP, full file retrieval, with refracs", {
  df <- make_small_vct() %>%
    select(-(ends_with("_q2.5") | ends_with("_q50") | ends_with("_q97.5")))
  want <- df %>%
    filter(q50) %>%
    select(date, D1, D2, fsc_small)
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  refracs <- data.frame(a = "lwr", b = "mid", c = "upr")
  # refracs should have no effect on OPP
  got <- read_parquet_one_quantile(path, 50, refracs = refracs)
  expect_equal(got, want)
})


test_that("read_parquet_one_quantile, OPP, selected columns", {
  df <- make_small_vct() %>%
    select(-(ends_with("_q2.5") | ends_with("_q50") | ends_with("_q97.5")))
  want <- df %>%
    filter(q50) %>%
    select(D1, fsc_small)
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  got <- read_parquet_one_quantile(path, 50, c("D1", "fsc_small"))
  expect_equal(got, want)
})

test_that("read_parquet_one_quantile, VCT, full file retrieval", {
  df <- make_small_vct()
  want <- df %>%
    filter(q50) %>%
    select(
      date, D1, D2, fsc_small,
      diam_lwr = diam_lwr_q50, diam_mid = diam_mid_q50, diam_upr = diam_upr_q50,
      Qc_lwr = Qc_lwr_q50, Qc_mid = Qc_mid_q50, Qc_upr = Qc_upr_q50,
      pop = pop_q50
    )
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  got <- read_parquet_one_quantile(path, 50)
  expect_equal(got, want)
})

test_that("read_parquet_one_quantile, VCT, selected columns", {
  df <- make_small_vct()
  want <- df %>%
    filter(q50) %>%
    select(
      date, D1, fsc_small,
      diam_lwr = diam_lwr_q50, diam_mid = diam_mid_q50, diam_upr = diam_upr_q50,
      pop = pop_q50
    )
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  got <- read_parquet_one_quantile(path, 50, c("date", "D1", "fsc_small", "diam", "pop"))
  expect_equal(got, want)
})

test_that("read_parquet_one_quantile, VCT, full file retrieval, refracs", {
  df <- make_small_vct()
  want <- df %>%
    filter(q50) %>%
    select(date, D1, D2, fsc_small)
  want$diam <- c(4, 5, 9, 11)
  want$Qc <- c(10, 11, 15, 17)
  want$pop <- c("a", "a", "b", "c")
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  refracs <- data.frame(a = "lwr", b = "mid", c = "upr")
  got <- read_parquet_one_quantile(path, 50, refracs = refracs)
  expect_equal(got, want)
})

test_that("read_parquet_one_quantile, VCT, selected columns, refracs", {
  df <- make_small_vct()
  want <- df %>%
    filter(q50) %>%
    select(date, D1, fsc_small)
  want$diam <- c(4, 5, 9, 11)
  want$Qc <- c(10, 11, 15, 17)
  want$pop <- c("a", "a", "b", "c")
  path <- withr::local_tempfile()
  arrow::write_parquet(df, path)
  refracs <- data.frame(a = "lwr", b = "mid", c = "upr")
  got <- read_parquet_one_quantile(
    path, 50, c("date", "D1", "fsc_small", "diam", "Qc", "pop"),
    refracs = refracs
  )
  expect_equal(got, want)

  # pop should be added automatically
  got <- read_parquet_one_quantile(
    path, 50, c("date", "D1", "fsc_small", "diam", "Qc"),
    refracs = refracs
  )
  expect_equal(got, want)

  # no diam
  got <- read_parquet_one_quantile(
    path, 50, c("date", "D1", "fsc_small", "Qc"),
    refracs = refracs
  )
  expect_equal(got, want %>% select(-c(diam)))
})

test_that("get_vct_range", {
  tempdir <- withr::local_tempdir()
  df <- make_range_vct(tempdir)
  vct_files <- list.files(tempdir, pattern = "*.parquet", full.names = TRUE)
  got <- get_vct_range(vct_files, c("pe", "diam"), 2.5)
  want <- tibble::tibble(pe = c(2, 7), diam_lwr = c(2, 11), diam_mid = c(3, 12), diam_upr = c(4, 13))
  expect_equal(got, want)
})

test_that("get_vct_range, one pop", {
  tempdir <- withr::local_tempdir()
  df <- make_range_vct(tempdir)
  vct_files <- list.files(tempdir, pattern = "*.parquet", full.names = TRUE)
  got <- get_vct_range(vct_files, c("pe", "diam"), 2.5, pop = "c")
  want <- tibble::tibble(pe = c(5, 7), diam_lwr = c(5, 11), diam_mid = c(6, 12), diam_upr = c(7, 13))
  expect_equal(got, want)
})

test_that("get_vct_range, with refracs", {
  tempdir <- withr::local_tempdir()
  df <- make_range_vct(tempdir)
  vct_files <- list.files(tempdir, pattern = "*.parquet", full.names = TRUE)
  refracs <- tibble::tibble(a = "upr", b = "lwr", c = "mid")
  got <- get_vct_range(vct_files, c("pe", "diam"), 2.5, refracs = refracs)
  want <- tibble::tibble(pe = c(2, 7), diam = c(3, 12))
  expect_equal(got, want)
})

test_that("get_vct_range, with refracs and one pop", {
  tempdir <- withr::local_tempdir()
  df <- make_range_vct(tempdir)
  vct_files <- list.files(tempdir, pattern = "*.parquet", full.names = TRUE)
  refracs <- tibble::tibble(a = "upr", b = "lwr", c = "mid")
  got <- get_vct_range(vct_files, c("pe", "diam"), 2.5, refracs = refracs, pop = "b")
  want <- tibble::tibble(pe = c(3, 6), diam = c(3, 10))
  expect_equal(got, want)
})