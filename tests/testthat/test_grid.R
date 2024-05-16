context("Gridding operations")
library(popcycle)
library(dplyr)

source("helper.R")

test_that("Grid two files", {
  x <- setUp()
  vct_dir <- x$grid.vct.dir
  db <- x$grid.db

  bin_count <- 5
  quantile_ <- 97.5
  qstr <- "q97.5"
  qsuffix <- "_q97.5"
  vct_files <- list.files(vct_dir, "\\.parquet$", full.names=T)
  meta <- create_meta(db, quantile_)
  refracs <- refracs <- tibble::tibble(
    prochloro="mid", synecho="mid", picoeuk="lwr", croco="lwr", beads="lwr", unknown="lwr"
  )
  calib <- tibble::tibble(pop=c("prochloro", "synecho"), a=c(2, 3), b=c(0, 1))
  grid_bins <- create_grid_bins(bin_count, log_base=2, log_answers=FALSE)
  # Create the gridded data
  gridded <- create_gridded(
    vct_files, quantile_, refracs, grid_bins, log_base=NULL, use_data.table=TRUE
  )
  # Don't use data.table
  gridded_no_dt <- create_gridded(
    vct_files, quantile_, refracs, grid_bins, log_base=NULL, use_data.table=FALSE
  )
  # Pass a vector of dates to ignore
  ignore_dates <- meta %>% dplyr::filter(flag != 0) %>% pull(date)
  gridded_ignore_dates <- create_gridded(
    vct_files, quantile_, refracs, grid_bins, log_base=NULL, use_data.table=TRUE,
    ignore_dates=ignore_dates
  )

  # Make sure using data.table and not using data.table yields the same result.
  # Exclude Qc_sum because of floating-point imprecision
  expect_true(all.equal(
    gridded %>% select(-c(Qc_sum)),
    gridded_no_dt %>% select(-c(Qc_sum)),
    check.attributes=FALSE
  ))
  # Test Qc_sum with near
  expect_true(all(near(gridded$Qc_sum, gridded_no_dt$Qc_sum)))

  # Get the actual VCT data
  vct <- lapply(vct_files, function(f) {
    vct <- arrow::read_parquet(f) %>%
      filter(get(qstr)) %>%
      select(date, fsc_small, chl_small, pe, ends_with(qsuffix)) %>%
      rename_with(                  # Remove quantile suffix from columns
        function(x) { stringr::str_replace(x, paste0(qsuffix, "$"), "") },
        ends_with(qsuffix)
      ) %>%
      mutate(
        diam=if_else(pop == "prochloro" | pop == "synecho", diam_mid, diam_lwr),
        Qc=if_else(pop == "prochloro" | pop == "synecho", Qc_mid, Qc_lwr)
      ) %>%
      select(-c(contains("lwr"), contains("mid"), contains("upr")))
  }) %>% bind_rows()

  # Total particles should be equal
  expect_equal(sum(gridded$n), nrow(vct))
  # Total Qc sum should be equal
  expect_equal(sum(gridded$Qc_sum), sum(vct$Qc))
  # There should not be any NAs
  expect_true(all(!is.na(gridded)))
  # All columns are present
  want_cols <- c("date", sapply(names(grid_bins), function(n) { paste0(n, "_coord") }), "pop", "n", "Qc_sum")
  names(want_cols) <- NULL
  expect_equal(names(gridded), want_cols)
  # All dates accounted for
  expect_equal(sort(unique(gridded$date)), sort(unique(vct$date)))
  # All coordinate indices within range
  expect_true(min(gridded %>% reframe(across(ends_with("_coord"), range))) >= 1)
  expect_true(max(gridded %>% reframe(across(ends_with("_coord"), range))) <= bin_count)

  # Dates are properly ignored
  expect_equal(sum(gridded_ignore_dates$n), vct %>% filter(! (date %in% ignore_dates)) %>% nrow())
  expect_true(length(intersect(gridded_ignore_dates$date, ignore_dates)) == 0)

  # Test a large random subset of rows to make sure n and Qc_sum add up properly
  # This is slow
  filter_vct <- function(vct, grid_bins, r) {
    # Find all VCT particles that match one gridded data row
    selection <- (
      vct$fsc_small >= grid_bins$fsc_small[r$fsc_small_coord] &
      vct$fsc_small < grid_bins$fsc_small[r$fsc_small_coord + 1] &
      vct$chl_small >= grid_bins$chl_small[r$chl_small_coord] &
      vct$chl_small < grid_bins$chl_small[r$chl_small_coord + 1] &
      vct$pe >= grid_bins$pe[r$pe_coord] &
      vct$pe < grid_bins$pe[r$pe_coord + 1] &
      vct$diam >= grid_bins$diam[r$diam_coord] &
      vct$diam < grid_bins$diam[r$diam_coord + 1] &
      vct$Qc >= grid_bins$Qc[r$Qc_coord] &
      vct$Qc < grid_bins$Qc[r$Qc_coord + 1] &
      vct$pop == r$pop &
      vct$date == r$date
    )
    return(vct[selection, ])
  }
  rows_to_test <- as.integer(nrow(gridded) * 0.10)  # 10% of test gridded data
  # set.seed(1)
  gridded_i_to_test <- sample(seq(nrow(gridded)), rows_to_test)
  n_answers <- sapply(gridded_i_to_test, function(i) {
    gridded[[i, "n"]] == filter_vct(vct, grid_bins, gridded[i, ]) %>% nrow()
  })
  expect_true(all(n_answers))
  # Qc_sum may not be exact because of floating-point imprecision, so use near
  # rather than ==
  Qc_sum_answers <- sapply(gridded_i_to_test, function(i) {
    near(gridded[[i, "Qc_sum"]], filter_vct(vct, grid_bins, gridded[i, ]) %>% pull(Qc) %>% sum())
  })
  expect_true(all(Qc_sum_answers))

  # Make sure boundary points are removed
  gridded_no_boundaries <- create_gridded(
    vct_files, quantile_, refracs, grid_bins, log_base=NULL, use_data.table=TRUE,
    max_boundary_proportion=0.2
  )
  n_removed <- vct %>%
    group_by(date) %>%
    group_modify( ~ {
      .x %>%
        filter(
          fsc_small == min(fsc_small) | fsc_small == max(fsc_small) |
          chl_small == min(chl_small) | chl_small == max(chl_small) |
          pe == min(pe) | pe == max(pe) |
          diam == min(diam) | diam == max(diam) |
          Qc == min(Qc) | Qc == max(Qc)
        )
    }) %>%
    group_split() %>%
    bind_rows() %>%
    nrow()
  expect_equal(sum(gridded_no_boundaries$n), nrow(vct) - n_removed)

  # Make sure boundary point cutoff is observed
  # No data should be returned
  gridded_no_boundaries_empty <- create_gridded(
    vct_files, quantile_, refracs, grid_bins, log_base=NULL, use_data.table=TRUE,
    max_boundary_proportion=0.01
  )
  expect_equal(nrow(gridded_no_boundaries_empty), 0)

  # Hourly data tests for one hour
  hourly <- group_gridded_by_time(gridded, time_expr="1 hours")
  expect_equal(length(unique(hourly$date)), 2)  # 2 hours of data
  seven_vct_n <- vct %>%
    filter(
      date >= lubridate::ymd_hms("2016-08-08 19:00:00"),
      date < lubridate::ymd_hms("2016-08-08 20:00:00")
    ) %>% nrow()
  expect_equal(
    hourly %>% filter(date == lubridate::ymd_hms("2016-08-08 19:00:00")) %>% pull(n) %>% sum(),
    seven_vct_n
  )
  seven_vct_Qc_sum <- vct %>%
    filter(
      date >= lubridate::ymd_hms("2016-08-08 19:00:00"),
      date < lubridate::ymd_hms("2016-08-08 20:00:00")
    ) %>% pull(Qc) %>% sum()
  expect_equal(
    hourly %>% filter(date == lubridate::ymd_hms("2016-08-08 19:00:00")) %>% pull(Qc_sum) %>% sum(),
    seven_vct_Qc_sum
  )

  # Test random rows in hourly data, slow
  filter_vct_hourly <- function(vct, grid_bins, r) {
    # Find all VCT particles that match one gridded data row for hourly data
    selection <- (
      vct$fsc_small >= grid_bins$fsc_small[r$fsc_small_coord] &
      vct$fsc_small < grid_bins$fsc_small[r$fsc_small_coord + 1] &
      vct$chl_small >= grid_bins$chl_small[r$chl_small_coord] &
      vct$chl_small < grid_bins$chl_small[r$chl_small_coord + 1] &
      vct$pe >= grid_bins$pe[r$pe_coord] &
      vct$pe < grid_bins$pe[r$pe_coord + 1] &
      vct$diam >= grid_bins$diam[r$diam_coord] &
      vct$diam < grid_bins$diam[r$diam_coord + 1] &
      vct$Qc >= grid_bins$Qc[r$Qc_coord] &
      vct$Qc < grid_bins$Qc[r$Qc_coord + 1] &
      vct$pop == r$pop &
      lubridate::floor_date(vct$date, "1 hour") == r$date
    )
    return(vct[selection, ])
  }
  rows_to_test <- as.integer(nrow(hourly) * 0.10)  # 10% of test gridded data
  set.seed(1)
  hourly_i_to_test <- sample(seq(nrow(hourly)), rows_to_test)
  n_answers <- sapply(hourly_i_to_test, function(i) {
    hourly[[i, "n"]] == filter_vct_hourly(vct, grid_bins, hourly[i, ]) %>% nrow()
  })
  expect_true(all(n_answers))
  # Qc_sum may not be exact because of floating-point imprecision, so use near
  # rather than ==
  Qc_sum_answers <- sapply(hourly_i_to_test, function(i) {
    near(hourly[[i, "Qc_sum"]], filter_vct_hourly(vct, grid_bins, hourly[i, ]) %>% pull(Qc) %>% sum())
  })
  expect_true(all(Qc_sum_answers))

  tearDown(x)
})

test_that("Volume table creation", {
  meta <- tibble::tibble(
    date=c(
      lubridate::ymd_hms("2016-08-08 19:33:41"),
      lubridate::ymd_hms("2016-08-08 19:36:41"),
      lubridate::ymd_hms("2016-08-08 20:33:41"),
      lubridate::ymd_hms("2016-08-08 20:36:41")
    ),
    volume=c(10000, 5000, 20000, 2000),
    opp_evt_ratio=c(0.01, 0.02, 0.03, 0.04)
  )

  # No time aggregation
  volumes <- popcycle::create_volume_table(meta, time_expr=NULL)
  want <- meta %>%
    mutate(
      volume_virtualcore=c(250, 125, 500, 50),
      volume_virtualcore_by_file=c(100, 100, 600, 80)
    ) %>%
    select(-c(opp_evt_ratio))
  expect_equal(volumes, want)

  # Hourly
  hourly <- popcycle::create_volume_table(meta)
  want <- tibble::tibble(
    date=c(
      lubridate::ymd_hms("2016-08-08 19:00:00"),
      lubridate::ymd_hms("2016-08-08 20:00:00")
    ),
    volume=c(15000, 22000),
    volume_virtualcore=c(375, 550),
    volume_virtualcore_by_file=c(200, 680)
  )
  expect_equal(hourly, want)

  # Fixed ratio
  volumes <- popcycle::create_volume_table(meta, time_expr=NULL, median_opp_evt_ratio=.01)
  want <- meta %>%
    mutate(
      volume_virtualcore=c(100, 50, 200, 20),
      volume_virtualcore_by_file=c(100, 100, 600, 80)
    ) %>%
    select(-c(opp_evt_ratio))
  expect_equal(volumes, want)
})

test_that("Abundance calculation", {
  gridded <- tibble::tibble(
    date=c(
      lubridate::ymd_hms("2016-08-08 19:00:00"),
      lubridate::ymd_hms("2016-08-08 19:00:00"),
      lubridate::ymd_hms("2016-08-08 19:00:00"),
      lubridate::ymd_hms("2016-08-08 20:00:00")
    ),
    fsc_small_coord=c(1,1,2,1), pe_coord=c(1,1,1,1), chl_small_coord=c(1,1,1,1),
    Qc_coord=c(1,1,1,1), diam_coord=c(1,1,1,1),
    pop=c("prochloro", "unknown", "prochloro", "synecho"),
    n=c(1, 2, 3, 4),
    Qc_sum=c(10, 20, 40, 40)
  )
  volumes <- tibble::tibble(
    date=c(
      lubridate::ymd_hms("2016-08-08 19:00:00"),
      lubridate::ymd_hms("2016-08-08 20:00:00")
    ),
    volume=c(15000, 22000),
    volume_virtualcore=c(375, 550),
    volume_virtualcore_by_file=c(200, 680)
  )

  # No calibration to influx data
  answers <- popcycle::add_abundance(gridded, volumes)
  want <- gridded %>%
    mutate(
      n_per_uL=c(1 / 200, 2 / 375, 3 / 200, 4 / 550),
      Qc_sum_per_uL=c(10 / 200, 20 / 375, 40 / 200, 40 / 550)
    )
  expect_equal(answers, want)

  # Test calibration
  calib <- tibble::tibble(pop=c("prochloro", "synecho"), a=c(2, 3))
  answers <- popcycle::add_abundance(gridded, volumes, calib=calib)
  want <- gridded %>%
    mutate(
      n_per_uL=c(2 * 1 / 200, 2 / 375, 2 * 3 / 200, 3 * 4 / 550),
      Qc_sum_per_uL=c(2 * 10 / 200, 20 / 375, 2 * 40 / 200, 3 * 40 / 550)
    )
  expect_equal(answers, want)
})

test_that("Boundary point removal for two columns", {
  df <- make_boundary_df()
  answer_idx <- c(
    c(2, 5, 6, 8),
    c(12, 13, 16, 17, 19, 20)
  )
  expect_equal(
    popcycle::remove_boundary_points(df, c("a", "b")),
    df[answer_idx, ]
  )
})

test_that("Boundary point removal for one column", {
  df <- make_boundary_df()
  answer_idx <- c(
    c(2, 3, 4, 5, 6, 8),
    c(12, 13, 15, 16, 17, 19, 20)
  )
  expect_equal(
    popcycle::remove_boundary_points(df, c("a")),
    df[answer_idx, ]
  )
})

test_that("Boundary point removal, max saturated only, for two columns", {
  df <- make_boundary_df()
  answer_idx <- c(
    c(1, 2, 3, 5, 6, 8, 10),
    c(12, 13, 16, 17, 18, 19, 20)
  )
  expect_equal(
    popcycle::remove_boundary_points(df, c("a", "b"), max_only = TRUE),
    df[answer_idx, ]
  )
})

test_that("Boundary point removal, max saturated only, for one column", {
  df <- make_boundary_df()
  answer_idx <- c(
    c(1, 2, 3, 4, 5, 6, 8, 10),
    c(11, 12, 13, 15, 16, 17, 18, 19, 20)
  )
  expect_equal(
    popcycle::remove_boundary_points(df, c("a"), max_only = TRUE),
    df[answer_idx, ]
  )
})

test_that("Boundary point removal, error on bad column", {
  df <- make_boundary_df()
  expect_error(popcycle::remove_boundary_points(df, c("a", "d")))
})

test_that("Boundary point removal, error when on date column", {
  df <- make_boundary_df()
  df$date <- NULL
  expect_error(popcycle::remove_boundary_points(df, c("a")))
})
