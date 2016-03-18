library(popcycle)
source("helper.R")

context("DB operations")

test_that("Load / Delete SFL", {
  x <- setUp()

  # Load SFL data
  save.sfl(x$db, x$cruise, evt.dir=x$evt.dir)
  sfl <- get.sfl.table(x$db)
  expect_true(nrow(sfl) == 5)
  reset.sfl.table(x$db)
  sfl <- get.sfl.table(x$db)
  expect_true(nrow(sfl) == 0)

  tearDown(x)
})

test_that("Save and retrieve old gating polygons", {
  x <- setUp()

  load("../params.RData")
  poly.log.oldest <- poly.log
  save.gating.params(x$db, poly.log.oldest)
  poly.log$beads[3, 1] <- 42.0
  # Save a bunch of params
  save.gating.params(x$db, poly.log)
  save.gating.params(x$db, poly.log)
  save.gating.params(x$db, poly.log)
  save.gating.params(x$db, poly.log)
  save.gating.params(x$db, poly.log)
  poly.log$beads[3, 1] <- 99.9
  save.gating.params(x$db, poly.log)
  gating.id.oldest <- get.gating.table(x$db)[1, "id"]
  gating.params.oldest <- get.gating.params.by.id(x$db, gating.id.oldest)
  gating.params.latest <- get.gating.params.latest(x$db)
  expect_equal(gating.params.oldest$poly.log, poly.log.oldest)
  expect_equal(gating.params.latest$poly.log, poly.log)

  tearDown(x)
})

test_that("Save and retrieve filter params", {
  x <- setUp()

  save.filter.params(x$db)
  save.filter.params(x$db, list(notch1=1, notch2=3, offset=100, origin=NA, width=1.0))
  save.filter.params(x$db, list(notch1=1, notch2=2, offset=100, origin=NA, width=1.0))
  save.filter.params(x$db, list(notch1=1, notch2=2, offset=100, origin=NA, width=1.0))
  save.filter.params(x$db, list(notch1=1, notch2=2, offset=200, origin=NA, width=1.0))
  oldest <- get.filter.table(x$db)[1, ]
  second.oldest <- get.filter.table(x$db)[2, ]
  second.oldest <- get.filter.params.by.id(x$db, second.oldest$id)
  latest <- get.filter.params.latest(x$db)

  expect_true(is.na(oldest$notch1))
  expect_true(is.na(oldest$notch2))
  expect_equal(oldest$offset, 0)
  expect_true(is.na(oldest$origin))
  expect_equal(oldest$width, 0.5)

  expect_equal(second.oldest$notch1, 1)
  expect_equal(second.oldest$notch2, 3)
  expect_equal(second.oldest$offset, 100)
  expect_true(is.na(second.oldest$origin))
  expect_equal(second.oldest$width, 1.0)

  expect_equal(latest$notch1, 1)
  expect_equal(latest$notch2, 2)
  expect_equal(latest$offset, 200)
  expect_true(is.na(latest$origin))
  expect_equal(latest$width, 1.0)

  tearDown(x)
})
