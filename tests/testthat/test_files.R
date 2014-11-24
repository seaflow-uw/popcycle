library(popcycle)

context("EVT file discovery")

test_that("EVT files are located correctly", {
  # Test EVT data is in inst folder
  save <- evt.location
  set.evt.location("../../inst/extdata")

  answer <- c("SeaFlow/datafiles/evt/2014_185/2014-07-04T00-00-02+00-00",
              "SeaFlow/datafiles/evt/2014_185/2014-07-04T00-03-02+00-00",
              "SeaFlow/datafiles/evt/2014_185/2014-07-04T00-06-02+00-00",
              "SeaFlow/datafiles/evt/2014_185/2014-07-04T00-09-02+00-00",
              "SeaFlow/datafiles/evt/2014_185/2014-07-04T00-12-02+00-00",
              "37.evt",
              "367.evt"
            )
  expect_equal(get.evt.list(), sort(answer))

  # Reset EVT location
  set.evt.location(save)
})
