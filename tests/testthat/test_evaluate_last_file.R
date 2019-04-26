context("Realtime last file pipeline")
library(popcycle)

source("helper.R")

test_that("Successfully run realtime last file pipeline", {
  x <- setUp()

  # Don't get latest EVT file. It absent from the SFL table in the test data
  # set. Use the first file instead.
  evt.file <- get.evt.files(x$evt.input.dir)[1]

  evaluate.evt(x$db.bare, x$evt.input.dir, x$opp.dir, x$vct.dir, evt.file)

  vct <- get.vct.table(x$db.bare)
  vct <- vct[order(vct$pop, vct$quantile), ]

  #print(paste0("vct$quantile = c(", paste(vct$quantile, collapse=" "), ")"))
  #print(paste0("vct$pop = c(", paste(vct$pop, collapse=" "), ")"))
  #print(paste0("vct$count = c(", paste(vct$count, collapse=" "), ")"))

  expect_equal(vct$quantile, c(2.5, 50, 97.5, 2.5, 50, 97.5, 2.5, 50, 97.5, 2.5, 50, 97.5))
  expect_equal(
    vct$pop,
    c("beads", "beads", "beads",
      "prochloro", "prochloro", "prochloro",
      "synecho", "synecho", "synecho",
      "unknown", "unknown", "unknown")
  )
  expect_equal(vct$count, c(7, 7, 7, 382, 72, 49, 20, 20, 19, 14, 8, 11))

  tearDown(x)
})
