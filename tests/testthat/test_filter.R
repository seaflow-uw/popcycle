context("EVT filtering")
library(popcycle)
library(dplyr)

test_that("Filter EVT files, one filter ID", {
  x <- setUp()

  evt_files <- get_evt_files(x$evt.input.dir)
  expect_equal(nrow(evt_files), 10)

  # Should filter EVT files that are also in the SFL table (8 of 10 files)
  expect_warning(
    filter_evt_files(x$db.bare, x$evt.input.dir, evt_files$path, x$opp.output.dir,
                     filter_id = "2414efe1-a4ff-46da-a393-9180d6eab149")
  )

  opp_table_expected <- get_opp_table(x$db.full.one, particles_in_all_quantiles = FALSE) %>%
    arrange(file_id, quantile)
  opp_table <- get_opp_table(x$db.bare, particles_in_all_quantiles = FALSE) %>%
    arrange(file_id, quantile)
  expect_equal(opp_table, opp_table_expected)

  # Then check that correct OPP files actually exist
  on_disk_files_expected <- c(
    "2014-07-04T00-00-00+00-00.1H.opp.parquet",
    "2014-07-04T01-00-00+00-00.1H.opp.parquet"
  )
  on_disk_files <- list.files(x$opp.output.dir, pattern=".*\\.1H\\.opp\\.parquet")
  expect_equal(on_disk_files, on_disk_files_expected)

  # Check OPP parquet file contents
  expect_true(all.equal(
    arrow::read_parquet(file.path(x$opp.output.dir, "2014-07-04T00-00-00+00-00.1H.opp.parquet")),
    arrow::read_parquet(file.path(x$opp.one.input.dir, "2014-07-04T00-00-00+00-00.1H.opp.parquet"))
  ))
  expect_true(all.equal(
    arrow::read_parquet(file.path(x$opp.output.dir, "2014-07-04T01-00-00+00-00.1H.opp.parquet")),
    arrow::read_parquet(file.path(x$opp.one.input.dir, "2014-07-04T01-00-00+00-00.1H.opp.parquet"))
  ))

  outlier_table_expected <- get_outlier_table(x$db.full.one) %>% arrange(file_id)
  outlier_table <- get_outlier_table(x$db.bare) %>% arrange(file_id)
  expect_equal(outlier_table, outlier_table_expected)
  expect_equal(sort(outlier_table$file_id), sort(unique(opp_table$file_id)))
  expect_equal(outlier_table$flag, rep(0L, length(unique(opp_table$file_id))))

  tearDown(x)
})


test_that("Filter EVT files, filter plan with two IDs", {
  x <- setUp()

  # Should filter EVT files that are also in the SFL table (8 of 10 files)
  expect_warning(
    filter_evt_files(x$db.bare, x$evt.input.dir, NULL, x$opp.output.dir)
  )

  opp_table_expected <- get_opp_table(x$db.full.plan, particles_in_all_quantiles = FALSE) %>%
    arrange(file_id, quantile)
  opp_table <- get_opp_table(x$db.bare, particles_in_all_quantiles = FALSE) %>%
    arrange(file_id, quantile)
  expect_equal(opp_table, opp_table_expected)

  # Then check that correct OPP files actually exist
  on_disk_files_expected <- c(
    "2014-07-04T00-00-00+00-00.1H.opp.parquet",
    "2014-07-04T01-00-00+00-00.1H.opp.parquet"
  )
  on_disk_files <- list.files(x$opp.output.dir, pattern=".*\\.1H\\.opp\\.parquet")
  expect_equal(on_disk_files, on_disk_files_expected)

  # Check OPP parquet file contents
  expect_true(all.equal(
    arrow::read_parquet(file.path(x$opp.output.dir, "2014-07-04T00-00-00+00-00.1H.opp.parquet")),
    arrow::read_parquet(file.path(x$opp.plan.input.dir, "2014-07-04T00-00-00+00-00.1H.opp.parquet"))
  ))
  expect_true(all.equal(
    arrow::read_parquet(file.path(x$opp.output.dir, "2014-07-04T01-00-00+00-00.1H.opp.parquet")),
    arrow::read_parquet(file.path(x$opp.plan.input.dir, "2014-07-04T01-00-00+00-00.1H.opp.parquet"))
  ))

  outlier_table_expected <- get_outlier_table(x$db.full.plan) %>% arrange(file_id)
  outlier_table <- get_outlier_table(x$db.bare) %>% arrange(file_id)
  expect_equal(outlier_table, outlier_table_expected)
  expect_equal(sort(outlier_table$file_id), sort(unique(opp_table$file_id)))
  expect_equal(outlier_table$flag, rep(0L, length(unique(opp_table$file_id))))

  tearDown(x)
})