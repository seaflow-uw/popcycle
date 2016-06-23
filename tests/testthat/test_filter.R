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
  row1.str <- "0.007775,0.885080147965475,0.876434676434676,0.0,-1792.0,0.5,1.16129192513726,357.456053525291,18.8299543891249,58.3402549599659,58.6423498778769,58.4252012096476,1.42497942517562,3.3801086782207,2.22204312828345,1.0,1269.15780524634,76.3087817349147,1.3345760374616,619.288485745662,20.8081339620631,53.3575327661354,53.64042220069,53.4667823956579"
  row2.str <- "0.00915,1.0123609394314,1.0,0.0,-1744.0,0.5,1.0158648592744,456.225638330021,19.6001640162515,58.3402549599659,58.6423498778769,58.4355397930196,1.48509053329249,3.51577041949972,2.19429520822524,1.0,3156.0618662238,121.626252207371,1.4399522258756,1808.52375977708,24.4732466062596,53.3969159678368,53.6734135072643,53.4683499999293"
  row1.answer <- unlist(lapply(unlist(strsplit(row1.str, ",")), as.double))
  row2.answer <- unlist(lapply(unlist(strsplit(row2.str, ",")), as.double))

  expect_equal(opp.stats[1, "filter_id"], filter.params$id)
  expect_equal(opp.stats[2, "filter_id"], filter.params$id)
  expect_equal(opp.stats[1, "file"], "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(opp.stats[2, "file"], "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp.stats[1, "opp_count"], 311)
  expect_equal(opp.stats[2, "opp_count"], 366)
  expect_equal(opp.stats[1, "evt_count"], 40000)
  expect_equal(opp.stats[2, "evt_count"], 40000)

  opp.stats.row1 <- unlist(opp.stats[1, 7:ncol(opp.stats)-1])
  opp.stats.row2 <- unlist(opp.stats[2, 7:ncol(opp.stats)-1])
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

test_that("Filter EVT files with non-default parameters", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.input.dir)
  save.filter.params(x$db, list(notch1=NA, notch2=NA, offset=1000, origin=NA, width=1))
  filter.params <- get.filter.params.latest(x$db)
  # Create a couple of newer filter parameter entries
  save.filter.params(x$db)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.input.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.input.dir, evt.files, x$opp.dir,
                                  filter.id=filter.params$id))
  opp.stats <- get.opp.table(x$db)

  # seaflowpy code answers for opp table, columns = opp_evt_ratio to chl_big_mean
  # which are the double type columns
  row1.str <- "0.084175,0.885080147965475,0.876434676434676,1000.0,-1792.0,1.0,1.00196947785185,1166.19845288663,10.5030323382336,58.2972257702318,58.6423498778769,58.4250334606439,1.41102962765284,3.78126155159547,2.24508455605578,1.0,1318.31139316244,32.8665286571316,1.25560797350002,2696.41126473463,12.4218027658325,53.3575327661354,53.64042220069,53.4623189662653"
  row2.str <- "0.094275,1.07058823529412,1.0,1000.0,-1744.0,1.0,1.00196947785185,3156.0618662238,16.4843606203092,58.2972257702318,58.6856338037586,58.4303112702247,1.18436708175812,9.57099371960924,2.19990481865988,1.0,3156.0618662238,42.4371893426448,1.4399522258756,3156.0618662238,15.2007130258795,53.3575327661354,53.6734135072643,53.46730713889"
  row1.answer <- unlist(lapply(unlist(strsplit(row1.str, ",")), as.double))
  row2.answer <- unlist(lapply(unlist(strsplit(row2.str, ",")), as.double))

  expect_equal(opp.stats[1, "filter_id"], filter.params$id)
  expect_equal(opp.stats[2, "filter_id"], filter.params$id)
  expect_equal(opp.stats[1, "file"], "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(opp.stats[2, "file"], "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp.stats[1, "opp_count"], 3367)
  expect_equal(opp.stats[2, "opp_count"], 3771)
  expect_equal(opp.stats[1, "evt_count"], 40000)
  expect_equal(opp.stats[2, "evt_count"], 40000)

  opp.stats.row1 <- unlist(opp.stats[1, 7:ncol(opp.stats)-1])
  opp.stats.row2 <- unlist(opp.stats[2, 7:ncol(opp.stats)-1])
  names(opp.stats.row1) <- NULL
  names(opp.stats.row2) <- NULL
  expect_equal(opp.stats.row1, row1.answer)
  expect_equal(opp.stats.row2, row2.answer)

  tearDown(x)
})
