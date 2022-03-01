context("Filtering and gating plan")
library(popcycle)
library(dplyr)

test_that("Save filter plan", {
  x <- setUp()

  file.copy(x$db.full.plan, file.path(x$tmp.dir, "test.db"))
  db <- file.path(x$tmp.dir, "test.db")

  # With date strings
  reset_filter_plan_table(db)
  expect_true(nrow(get_filter_plan_table(db)) == 0)
  plan <- tibble::tibble(
    start_date = c("2022-01-01T00:00:00+00:00", "2022-01-01T01:00:00+00:00"),
    filter_id = c("2414efe1-a4ff-46da-a393-9180d6eab149", "ac874650-9b4b-4db1-8e3c-4e75aebfab6e")
  )
  save_filter_plan(db, plan)
  expect_equal(
    get_filter_plan_table(db),
    plan %>% dplyr::mutate(start_date = lubridate::ymd_hms(start_date))
  )

  # With dates
  reset_filter_plan_table(db)
  expect_true(nrow(get_filter_plan_table(db)) == 0)
  plan_dates <- plan %>% mutate(start_date = lubridate::ymd_hms(start_date))
  save_filter_plan(db, plan_dates)
  expect_equal(get_filter_plan_table(db), plan_dates)

  # Overwrites
  reset_filter_plan_table(db)
  save_filter_plan(db, plan)
  new_plan <- plan %>%
    mutate(start_date = c("2050-01-01T00:00:00+00:00", "2050-01-01T01:00:00+00:00"))
  save_filter_plan(db, new_plan)
  expect_equal(
    get_filter_plan_table(db),
    new_plan %>% dplyr::mutate(start_date = lubridate::ymd_hms(start_date))
  )

  # With wrong type for dates
  reset_filter_plan_table(db)
  expect_true(nrow(get_filter_plan_table(db)) == 0)
  plan_bad <- plan %>% mutate(start_date = as.numeric(lubridate::ymd_hms(start_date)))
  expect_error(save_filter_plan(db, plan_bad))

  # Bad date string
  reset_filter_plan_table(db)
  expect_true(nrow(get_filter_plan_table(db)) == 0)
  plan_bad <- plan %>% mutate(start_date = "---")
  expect_error(save_filter_plan(db, plan_bad))

  # NA start_date
  plan_bad <- plan %>% mutate(start_date = NA)
  expect_error(save_filter_plan(db, plan_bad))

  # filter_id not in db
  plan_bad <- plan %>% mutate(filter_id = "not in db")
  expect_error(save_filter_plan(db, plan_bad))

  tearDown(x)
})


test_that("Save gating plan", {
  x <- setUp()

  file.copy(x$db.full.plan, file.path(x$tmp.dir, "test.db"))
  db <- file.path(x$tmp.dir, "test.db")

  # With date strings
  reset_gating_plan_table(db)
  expect_true(nrow(get_gating_plan_table(db)) == 0)
  plan <- tibble::tibble(
    start_date = c("2022-01-01T00:00:00+00:00", "2022-01-01T01:00:00+00:00"),
    gating_id = c("0e283d1c-ba99-4dca-99bb-35735eeafc48", "9eeb089d-16d3-454d-b7dd-d358b078ff51")
  )
  save_gating_plan(db, plan)
  expect_equal(
    get_gating_plan_table(db),
    plan %>% dplyr::mutate(start_date = lubridate::ymd_hms(start_date))
  )

  # With dates
  reset_gating_plan_table(db)
  expect_true(nrow(get_gating_plan_table(db)) == 0)
  plan_dates <- plan %>% mutate(start_date = lubridate::ymd_hms(start_date))
  save_gating_plan(db, plan_dates)
  expect_equal(get_gating_plan_table(db), plan_dates)

  # Overwrites
  reset_gating_plan_table(db)
  save_gating_plan(db, plan)
  new_plan <- plan %>%
    mutate(start_date = c("2050-01-01T00:00:00+00:00", "2050-01-01T01:00:00+00:00"))
  save_gating_plan(db, new_plan)
  expect_equal(
    get_gating_plan_table(db),
    new_plan %>% dplyr::mutate(start_date = lubridate::ymd_hms(start_date))
  )

  # With wrong type for dates
  reset_gating_plan_table(db)
  expect_true(nrow(get_gating_plan_table(db)) == 0)
  plan_bad <- plan %>% mutate(start_date = as.numeric(lubridate::ymd_hms(start_date)))
  expect_error(save_gating_plan(db, plan_bad))

  # Bad date string
  reset_gating_plan_table(db)
  expect_true(nrow(get_gating_plan_table(db)) == 0)
  plan_bad <- plan %>% mutate(start_date = "---")
  expect_error(save_gating_plan(db, plan_bad))

  # NA start_date
  plan_bad <- plan %>% mutate(start_date = NA)
  expect_error(save_gating_plan(db, plan_bad))

  # gating_id not in db
  plan_bad <- plan %>% mutate(gating_id = "not in db")
  expect_error(save_gating_plan(db, plan_bad))

  tearDown(x)
})