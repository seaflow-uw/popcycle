library(popcycle)

context("EVT filtering")

test_that("Filter EVT files", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.input.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.input.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.input.dir, evt.files, x$opp.dir))
  filter.params <- get.filter.params.latest(x$db)
  opp.stats <- get.opp.table(x$db)

  # seaflowpy code answers for opp table, columns = opp_evt_ratio to chl_big_mean
  # which are the double type columns
  row1.str <- "0.0628562123432666,0.885080147965475,0.876434676434676,0.0,-752.0,1.0,1.16129192513726,1166.19845288663,29.1071323895189,58.3402549599659,58.6423498778769,58.4356284567576,1.42497942517562,3.3801086782207,2.21323606685022,1.0,1318.31139316244,115.907652918693,1.3345760374616,1540.00768539102,29.4146520420598,53.3575327661354,53.64042220069,53.4706973634835"
  row2.str <- "0.0588901472253681,1.07058823529412,1.0,0.0,-624.0,1.0,1.205526489704,3156.0618662238,35.5586666341674,58.3402549599659,58.6856338037586,58.4423439401207,1.48509053329249,4.38251723606804,2.19714549620276,1.0,3156.0618662238,157.603283554857,1.4399522258756,1808.52375977708,36.540596884212,53.3969159678368,53.6734135072643,53.4733259158674"
  row1.answer <- unlist(lapply(unlist(strsplit(row1.str, ",")), as.double))
  row2.answer <- unlist(lapply(unlist(strsplit(row2.str, ",")), as.double))

  expect_equal(opp.stats[1, "filter_id"], filter.params$id)
  expect_equal(opp.stats[2, "filter_id"], filter.params$id)
  expect_equal(opp.stats[1, "file"], "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(opp.stats[2, "file"], "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp.stats[1, "opp_count"], 386)
  expect_equal(opp.stats[2, "opp_count"], 416)
  expect_equal(opp.stats[1, "all_count"], 40000)
  expect_equal(opp.stats[2, "all_count"], 40000)
  expect_equal(opp.stats[1, "evt_count"], 6141)
  expect_equal(opp.stats[2, "evt_count"], 7064)

  opp.stats.row1 <- unlist(opp.stats[1, 8:ncol(opp.stats)-1])
  opp.stats.row2 <- unlist(opp.stats[2, 8:ncol(opp.stats)-1])
  names(opp.stats.row1) <- NULL
  names(opp.stats.row2) <- NULL
  expect_equal(opp.stats.row1, row1.answer)
  expect_equal(opp.stats.row2, row2.answer)

  # Make sure written OPP files have the same number of particles as those
  # recorded in database opp table
  opp.files <- get.opp.files(x$db)
  i <- 1
  for (opp.file in opp.files) {
    opp <- get.opp.by.file(x$opp.dir, opp.file)
    expect_equal(opp.stats[i, "opp_count"], nrow(opp))
    i <- i + 1
  }

  tearDown(x)
})
