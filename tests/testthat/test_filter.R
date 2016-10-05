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
  row1.str <- "0.00991785213384091,0.885080147965475,0.876434676434676,0.0,-1808.0,1.0,1.16129192513726,1166.19845288663,27.8747912697262,58.3402549599659,58.6423498778769,58.4343242286785,1.42497942517562,3.62107808977048,2.21015996021373,1.0,1318.31139316244,110.169253465082,1.30857434291842,1540.00768539102,28.3315681040607,53.3575327661354,53.64042220069,53.4696464151013"
  row2.str <- "0.0104696305572949,1.07058823529412,1.0,0.0,-1760.0,1.0,1.0158648592744,3156.0618662238,33.894835202603,58.3402549599659,58.6856338037586,58.4411841063085,1.42497942517562,4.38251723606804,2.19345733764844,1.0,3156.0618662238,150.851237328772,1.4399522258756,1808.52375977708,32.0042709892765,53.3969159678368,53.6734135072643,53.4724400528629"
  row1.answer <- unlist(lapply(unlist(strsplit(row1.str, ",")), as.double))
  row2.answer <- unlist(lapply(unlist(strsplit(row2.str, ",")), as.double))

  expect_equal(opp.stats[1, "filter_id"], filter.params$id)
  expect_equal(opp.stats[2, "filter_id"], filter.params$id)
  expect_equal(opp.stats[1, "file"], "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(opp.stats[2, "file"], "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp.stats[1, "opp_count"], 396)
  expect_equal(opp.stats[2, "opp_count"], 418)
  expect_equal(opp.stats[1, "all_count"], 40000)
  expect_equal(opp.stats[2, "all_count"], 40000)
  expect_equal(opp.stats[1, "evt_count"], 39928)
  expect_equal(opp.stats[2, "evt_count"], 39925)

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
