library(popcycle)

context("EVT filtering")

test_that("Filter EVT files", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))
  filter.params <- get.filter.params.latest(x$db)
  opp.stats <- get.opp.table(x$db)

  # seaflowpy code answers for opp table, columns = opp_evt_ratio to chl_big_mean
  # which are the double type columns
  row1.str <- "0.008625,0.766880341880342,0.760381355932203,0,-1792,0.5,1.16129192513726,1166.19845288663,23.1879033296803,58.3402549599659,58.6423498778769,58.4258231308456,1.42497942517562,3.3801086782207,2.22384853817237,1,1269.15780524634,74.8432875310062,1.3345760374616,922.190962524502,22.2663674297412,53.3575327661354,53.64042220069,53.4659543179773"
  row2.str <- "0.0101,0.876873661670236,0.867584745762712,0,-1744,0.5,1.0158648592744,3156.0618662238,50.2361959272678,58.3402549599659,58.6423498778769,58.4382240324954,1.48509053329249,9.57099371960924,2.24165272486415,1,3156.0618662238,123.582175718135,1.4399522258756,3156.0618662238,39.7773250707217,53.3969159678368,53.6734135072643,53.4701083186048"
  row3.str <- "0.009125,0.792992950224311,0.786440677966102,0,-432,0.5,1.14766319202265,1485.50801717277,40.0813101646839,58.3402549599659,58.6423498778769,58.4561458103905,1.41938303271921,3.45406159668612,2.14102114572482,1,2710.70702052511,119.295083618405,1.53466033368159,3156.0618662238,49.1229950576066,53.3575327661354,53.64042220069,53.4772304627338"
  row1.answer <- unlist(lapply(unlist(strsplit(row1.str, ",")), as.double))
  row2.answer <- unlist(lapply(unlist(strsplit(row2.str, ",")), as.double))
  row3.answer <- unlist(lapply(unlist(strsplit(row3.str, ",")), as.double))

  expect_equal(opp.stats[1, "filter_id"], filter.params$id)
  expect_equal(opp.stats[2, "filter_id"], filter.params$id)
  expect_equal(opp.stats[3, "filter_id"], filter.params$id)
  expect_equal(opp.stats[1, "file"], "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(opp.stats[2, "file"], "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp.stats[3, "file"], "2014_185/2014-07-04T00-12-02+00-00")
  expect_equal(opp.stats[1, "opp_count"], 345)
  expect_equal(opp.stats[2, "opp_count"], 404)
  expect_equal(opp.stats[3, "opp_count"], 365)
  expect_equal(opp.stats[1, "evt_count"], 40000)
  expect_equal(opp.stats[2, "evt_count"], 40000)
  expect_equal(opp.stats[3, "evt_count"], 40000)

  opp.stats.row1 <- unlist(opp.stats[1, 7:ncol(opp.stats)-1])
  opp.stats.row2 <- unlist(opp.stats[2, 7:ncol(opp.stats)-1])
  opp.stats.row3 <- unlist(opp.stats[3, 7:ncol(opp.stats)-1])
  names(opp.stats.row1) <- NULL
  names(opp.stats.row2) <- NULL
  names(opp.stats.row3) <- NULL
  expect_equal(opp.stats.row1, row1.answer)
  expect_equal(opp.stats.row2, row2.answer)
  expect_equal(opp.stats.row3, row3.answer)

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

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db, list(notch1=NA, notch2=NA, offset=1000, origin=NA, width=1))
  filter.params <- get.filter.params.latest(x$db)
  # Create a couple of newer filter parameter entries
  save.filter.params(x$db)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir,
                                  filter.id=filter.params$id))
  opp.stats <- get.opp.table(x$db)

  # seaflowpy code answers for opp table, columns = opp_evt_ratio to chl_big_mean
  # which are the double type columns
  row1.str <- "0.084175,0.766880341880342,0.760381355932203,1000,-1792,1,1.00196947785185,1166.19845288663,10.5030323382336,58.2972257702318,58.6423498778769,58.4250334606439,1.41102962765284,3.78126155159547,2.24508455605578,1,1318.31139316244,32.8665286571316,1.25560797350002,2696.41126473463,12.4218027658325,53.3575327661354,53.64042220069,53.4623189662653"
  row2.str <- "0.094275,0.920224719101124,0.867584745762712,1000,-1744,1,1.00196947785185,3156.0618662238,16.4843606203092,58.2972257702318,58.6856338037586,58.4303112702247,1.18436708175812,9.57099371960924,2.19990481865988,1,3156.0618662238,42.4371893426448,1.4399522258756,3156.0618662238,15.2007130258795,53.3575327661354,53.6734135072643,53.46730713889"
  row3.str <- "0.093325,0.792992950224311,0.786440677966102,1000,-432,1,1.00196947785185,1485.50801717277,15.5067840976822,58.3402549599659,58.6423498778769,58.4440021568292,1.22706392280062,3.60685684997739,2.148181243045,1,3156.0618662238,43.2821281265747,1.44562971925738,3156.0618662238,17.3342519532111,53.3575327661354,53.7130298605807,53.471562895211"
  row1.answer <- unlist(lapply(unlist(strsplit(row1.str, ",")), as.double))
  row2.answer <- unlist(lapply(unlist(strsplit(row2.str, ",")), as.double))
  row3.answer <- unlist(lapply(unlist(strsplit(row3.str, ",")), as.double))

  expect_equal(opp.stats[1, "filter_id"], filter.params$id)
  expect_equal(opp.stats[2, "filter_id"], filter.params$id)
  expect_equal(opp.stats[3, "filter_id"], filter.params$id)
  expect_equal(opp.stats[1, "file"], "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(opp.stats[2, "file"], "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp.stats[3, "file"], "2014_185/2014-07-04T00-12-02+00-00")
  expect_equal(opp.stats[1, "opp_count"], 3367)
  expect_equal(opp.stats[2, "opp_count"], 3771)
  expect_equal(opp.stats[3, "opp_count"], 3733)
  expect_equal(opp.stats[1, "evt_count"], 40000)
  expect_equal(opp.stats[2, "evt_count"], 40000)
  expect_equal(opp.stats[3, "evt_count"], 40000)

  opp.stats.row1 <- unlist(opp.stats[1, 7:ncol(opp.stats)-1])
  opp.stats.row2 <- unlist(opp.stats[2, 7:ncol(opp.stats)-1])
  opp.stats.row3 <- unlist(opp.stats[3, 7:ncol(opp.stats)-1])
  names(opp.stats.row1) <- NULL
  names(opp.stats.row2) <- NULL
  names(opp.stats.row3) <- NULL
  expect_equal(opp.stats.row1, row1.answer)
  expect_equal(opp.stats.row2, row2.answer)
  expect_equal(opp.stats.row3, row3.answer)

  tearDown(x)
})
