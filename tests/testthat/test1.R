# Tests for popcycle
library(popcycle)

context("File exploration")

test_that("EVT files are located correctly", {
  # Test EVT data is in inst folder
  cur.evt.location <- evt.location
  set.evt.location("../../inst/extdata")

  answer <- c("SeaFlow/datafiles/evt/2014_135/2014-05-15T17-07-08+0000",
              "SeaFlow/datafiles/evt/2014_135/2014-05-15T17-10-09+0000",
              "SeaFlow/datafiles/evt/2014_135/2014-05-15T17-13-09+0000",
              "SeaFlow/datafiles/evt/2014_135/2014-05-15T17-16-09+0000",
              "SeaFlow/datafiles/evt/2014_135/2014-05-15T17-19-09+0000",
              "37.evt",
              "367.evt"
            )
  expect_equal(get.evt.list(), sort(answer))

  # Set EVT location back to default
  set.evt.location(cur.evt.location)
})

context("Project configuration")

test_that("Project folder is created correctly", {
  cur.project.location <- get.project.location()
  newdir <- tempdir()
  projdir <- file.path(newdir, "project")
  set.project.location(projdir)
  expect
  set.project.location(cur.project.location)
})
