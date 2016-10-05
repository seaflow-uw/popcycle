library(popcycle)
source("helper.R")

context("Realtime last file pipeline")

test_that("Successfully run realtime last file pipeline", {
  x <- setUp()

  save.sfl(x$db, x$cruise, evt.dir=x$evt.input.dir)
  # Don't get latest EVT file. It's actually empty in test data set. Use
  # the first file instead.
  evt.file <- get.evt.files(x$evt.input.dir)[1]

  save.filter.params(x$db)
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  evaluate.evt(x$db, x$cruise, x$evt.input.dir, x$opp.dir, x$vct.dir, evt.file)

  stats <- get.stat.table(x$db)

  print(paste0("stats$pop = c(", paste(stats$pop, collapse=" "), ")"))
  print(paste0("stats$n_count = c(", paste(stats$n_count, collapse=" "), ")"))

  expect_equal(stats$pop, c("beads", "picoeuk", "prochloro", "synecho", "unknown"))
  expect_equal(stats$n_count, c(60, 20, 227, 84, 5))

  tearDown(x)
})
