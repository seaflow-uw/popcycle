context("PSD gridding operations")
library(popcycle)
library(dplyr)

source("helper.R")

test_that("Grid two files", {
  x <- setUp()
  bins <- 5
  quantile_ <- 97.5
  qstr <- "q97.5"
  qsuffix <- "_q97.5"
  vct_files <- list.files(x$psd.vct.dir, "\\.parquet$", full.names=T)
  meta <- create_meta(x$psd.db, quantile_)
  refracs <- refracs <- tibble::tibble(
    prochloro="mid", synecho="mid", picoeuk="lwr", croco="lwr", beads="lwr", unknown="lwr"
  )
  calib <- tibble::tibble(pop=c("prochloro", "synecho"), a=c(2, 3), b=c(0, 1))
  grid <- create_grid(bins, log_base=2, log_answers=FALSE)
  # Create the gridded data
  psd <- create_PSD(
    vct_files, quantile_, refracs, grid, log_base=NULL, use_data.table=TRUE
  )
  # Don't use data.table
  psd_no_dt <- create_PSD(
    vct_files, quantile_, refracs, grid, log_base=NULL, use_data.table=FALSE
  )

  # Make sure using data.table and not using data.table yields the same result.
  # Exclude Qc_sum because of floating-point imprecision
  expect_true(all_equal(
    psd %>% select(-c(Qc_sum)),
    psd_no_dt %>% select(-c(Qc_sum))
  ))
  # Test Qc_sum with near
  expect_true(all(near(psd$Qc_sum, psd_no_dt$Qc_sum)))

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
  expect_equal(sum(psd$n), nrow(vct))
  # Total Qc sum should be equal
  expect_equal(sum(psd$Qc_sum), sum(vct$Qc))
  # There should not be any NAs
  expect_true(all(!is.na(psd)))
  # All columns are present
  want_cols <- c("date", sapply(names(grid), function(n) { paste0(n, "_coord") }), "pop", "n", "Qc_sum")
  names(want_cols) <- NULL
  expect_equal(names(psd), want_cols)
  # All dates accounted for
  expect_equal(sort(unique(psd$date)), sort(unique(vct$date)))
  # All coordinate indices within range
  expect_true(min(psd %>% select(ends_with("_coord")) %>% summarise_all(range)) >= 1)
  expect_true(max(psd %>% select(ends_with("_coord")) %>% summarise_all(range)) <= bins)

  # Test a large random subset of rows to make sure n and Qc_sum add up properly
  # This is slow
  filter_vct <- function(vct, grid, r) {
    # Find all VCT particles that match one PSD gridded data row
    return(vct %>% filter(
      fsc_small >= grid$fsc_small[r$fsc_small_coord],
      fsc_small < grid$fsc_small[r$fsc_small_coord + 1],
      chl_small >= grid$chl_small[r$chl_small_coord],
      chl_small < grid$chl_small[r$chl_small_coord + 1],
      pe >= grid$pe[r$pe_coord],
      pe < grid$pe[r$pe_coord + 1],
      diam >= grid$diam[r$diam_coord],
      diam < grid$diam[r$diam_coord + 1],
      Qc >= grid$Qc[r$Qc_coord],
      Qc < grid$Qc[r$Qc_coord + 1],
      pop == r$pop,
      date == r$date
    ))
  }
  rows_to_test <- as.integer(nrow(psd) * 0.10)  # 10% of test gridded data
  set.seed(1)
  psd_i_to_test <- sample(seq(nrow(psd)), rows_to_test)
  n_answers <- sapply(psd_i_to_test, function(i) {
    psd[[i, "n"]] == filter_vct(vct, grid, psd[i, ]) %>% nrow()
  })
  expect_true(all(n_answers))
  # Qc_sum may not be exact because of floating-point imprecision, so use near
  # rather than ==
  Qc_sum_answers <- sapply(psd_i_to_test, function(i) {
    near(psd[[i, "Qc_sum"]], filter_vct(vct, grid, psd[i, ]) %>% pull(Qc) %>% sum())
  })
  expect_true(all(Qc_sum_answers))

  # Make sure boundary points are removed
  psd_no_boundaries <- create_PSD(
    vct_files, quantile_, refracs, grid, log_base=NULL, use_data.table=TRUE,
    remove_boundary_points=TRUE
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
  expect_equal(sum(psd_no_boundaries$n), nrow(vct) - n_removed)

  # Hourly data tests for one hour
  hourly <- group_psd_by_time(psd, time_expr="1 hours")
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
  filter_vct_hourly <- function(vct, grid, r) {
    # Find all VCT particles that match one PSD gridded data row for hourly data
    return(vct %>% filter(
      fsc_small >= grid$fsc_small[r$fsc_small_coord],
      fsc_small < grid$fsc_small[r$fsc_small_coord + 1],
      chl_small >= grid$chl_small[r$chl_small_coord],
      chl_small < grid$chl_small[r$chl_small_coord + 1],
      pe >= grid$pe[r$pe_coord],
      pe < grid$pe[r$pe_coord + 1],
      diam >= grid$diam[r$diam_coord],
      diam < grid$diam[r$diam_coord + 1],
      Qc >= grid$Qc[r$Qc_coord],
      Qc < grid$Qc[r$Qc_coord + 1],
      pop == r$pop,
      lubridate::floor_date(date, "1 hour") == r$date
    ))
  }
  rows_to_test <- as.integer(nrow(hourly) * 0.10)  # 10% of test gridded data
  set.seed(1)
  hourly_i_to_test <- sample(seq(nrow(hourly)), rows_to_test)
  n_answers <- sapply(hourly_i_to_test, function(i) {
    hourly[[i, "n"]] == filter_vct_hourly(vct, grid, hourly[i, ]) %>% nrow()
  })
  expect_true(all(n_answers))
  # Qc_sum may not be exact because of floating-point imprecision, so use near
  # rather than ==
  Qc_sum_answers <- sapply(hourly_i_to_test, function(i) {
    near(hourly[[i, "Qc_sum"]], filter_vct_hourly(vct, grid, hourly[i, ]) %>% pull(Qc) %>% sum())
  })
  expect_true(all(Qc_sum_answers))

  tearDown(x)
})

# TODO test calib and abundance calcs
# TODO boundary points test