library(popcycle)
source("helper.R")

context("Realtime last file pipeline")

test_that("Successfully run realtime last file pipeline", {
  x <- setUp()

  save.sfl(x$db, x$cruise, evt.dir=x$evt.dir)
  evt.file <- get.latest.evt.with.day(x$evt.dir)

  save.filter.params(x$db)
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  evaluate.evt(x$db, x$cruise, x$evt.dir, x$opp.dir, x$vct.dir, evt.file)

  stats <- get.stat.table(x$db)

  print(paste0("stats$pop = c(", paste(stats$pop, collapse=" "), ")"))
  print(paste0("stats$n_count = c(", paste(stats$n_count, collapse=" "), ")"))

  expect_equal(stats$pop, c("beads", "picoeuk", "prochloro", "synecho", "unknown"))
  expect_equal(stats$n_count, c(39, 33, 206, 47, 40))

  tearDown(x)
})
