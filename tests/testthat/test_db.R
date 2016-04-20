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

  cols <- c(3,4,5,6,7)
  oldest <- get.filter.table(x$db)[1, cols]
  second.oldest <- get.filter.table(x$db)[2, ]
  second.oldest <- get.filter.params.by.id(x$db, second.oldest$id)[1, cols]
  latest <- get.filter.params.latest(x$db)[1, cols]

  expect_equal(oldest, data.frame(list(notch1=as.double(NA), notch2=as.double(NA), offset=0, origin=as.double(NA), width=0.5)))
  expect_equal(second.oldest, data.frame(list(notch1=1, notch2=3, offset=100, origin=as.double(NA), width=1.0)))
  expect_equal(latest, data.frame(list(notch1=1, notch2=2, offset=200, origin=as.double(NA), width=1.0)))

  tearDown(x)
})

test_that("Retrieve OPP stats by file", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))

  opp2 <- get.opp.stats.by.file(x$db, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(opp2$opp_count, 404)
  expect_equal(opp2$file, "2014_185/2014-07-04T00-03-02+00-00")
})

test_that("Retrieve VCT stats by file", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  classify.opp.files(x$db, x$cruise, x$opp.dir, get.opp.files(x$db), x$vct.dir)

  vct2 <- get.vct.stats.by.file(x$db, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(vct2$pop, c("beads", "picoeuk", "prochloro", "synecho", "unknown"))
  expect_equal(vct2$count, c(45, 24, 256, 45, 34))
})

test_that("Retrieve OPP stats by date", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))

  opp12 <- get.opp.stats.by.date(x$db, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(opp12$file, c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"))
})

test_that("Retrieve VCT stats by file", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  classify.opp.files(x$db, x$cruise, x$opp.dir, get.opp.files(x$db), x$vct.dir)

  vct2 <- get.vct.stats.by.file(x$db, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(vct2$pop, c("beads", "picoeuk", "prochloro", "synecho", "unknown"))
  expect_equal(vct2$count, c(45, 24, 256, 45, 34))
})

test_that("Retrieve VCT stats by date", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  classify.opp.files(x$db, x$cruise, x$opp.dir, get.opp.files(x$db), x$vct.dir)

  vct12 <- get.vct.stats.by.date(x$db, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(
    vct12$file,
    c(rep("2014_185/2014-07-04T00-00-02+00-00", 5), rep("2014_185/2014-07-04T00-03-02+00-00", 5))
  )
})

test_that("Retrieve OPP by file", {
  x <- setUp()
  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  classify.opp.files(x$db, x$cruise, x$opp.dir, get.opp.files(x$db), x$vct.dir)

  # Without VCT, transformed
  opp <- get.opp.by.file(x$opp.dir, get.opp.files(x$db)[1:2])
  expect_equal(nrow(opp), 749)
  expect_true(!any(max(opp[, length(popcycle:::EVT.HEADER)]) > 10^3.5))

  # Without VCT, not transformed
  opp <- get.opp.by.file(x$opp.dir, get.opp.files(x$db)[1:2], transform=F)
  expect_equal(nrow(opp), 749)
  expect_true(any(max(opp[, seq(3, length(popcycle:::EVT.HEADER))]) > 10^3.5))

  # With VCT
  opp <- get.opp.by.file(x$opp.dir, get.opp.files(x$db)[1:2], vct.dir=x$vct.dir)
  expect_true("pop" %in% names(opp))
})

test_that("Retrieve OPP by date", {
  x <- setUp()

  evt.files <- get.evt.files(x$evt.dir)
  save.filter.params(x$db)
  save.sfl(x$db, x$cruise, x$evt.dir)
  expect_warning(filter.evt.files(x$db, x$cruise, x$evt.dir, evt.files, x$opp.dir))
  load("../params.RData")
  save.gating.params(x$db, poly.log)
  classify.opp.files(x$db, x$cruise, x$opp.dir, get.opp.files(x$db), x$vct.dir)

  # Without VCT, transformed
  opp <- get.opp.by.date(x$db, x$opp.dir, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(nrow(opp), 749)
  expect_true(!any(max(opp[, length(popcycle:::EVT.HEADER)]) > 10^3.5))

  # Without VCT, not transformed
  opp <- get.opp.by.date(x$db, x$opp.dir, "2014-07-04 00:00", "2014-07-04 00:04",
                         transform=F)
  expect_equal(nrow(opp), 749)
  expect_true(any(max(opp[, seq(3, length(popcycle:::EVT.HEADER))]) > 10^3.5))

  # With VCT
  opp <- get.opp.by.date(x$db, x$opp.dir, "2014-07-04 00:00", "2014-07-04 00:04",
                         vct.dir=x$vct.dir)
  expect_true("pop" %in% names(opp))
})

test_that("Retrieve EVT file names by date", {
  x <- setUp()

  save.sfl(x$db, x$cruise, evt.dir=x$evt.dir)
  evt.files <- get.evt.files.by.date(x$db, x$evt.dir, "2014-07-04 00:03", "2014-07-04 00:10")
  answer <- c(
    "2014_185/2014-07-04T00-03-02+00-00",
    "2014_185/2014-07-04T00-06-02+00-00",
    "2014_185/2014-07-04T00-09-02+00-00"
  )
  expect_equal(evt.files, answer)
})
