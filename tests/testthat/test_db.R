context("DB operations")
library(popcycle)

source("helper.R")

test_that("Load / Delete SFL", {
  x <- setUp()

  # Load SFL data
  save.sfl(x$db, sfl.file=x$sfl.file, cruise=x$cruise, inst=x$serial)
  sfl <- get.sfl.table(x$db)
  expect_true(nrow(sfl) == 8)
  reset.sfl.table(x$db)
  sfl <- get.sfl.table(x$db)
  expect_true(nrow(sfl) == 0)

  tearDown(x)
})

test_that("Save and retrieve filter params", {
  x <- setUp()

  filter.params1 <- data.frame(
    quantile=50.0,
    beads.fsc.small=1,
    beads.D1=2,
    beads.D2=3,
    width=4,
    notch.small.D1=5,
    notch.small.D2=6,
    notch.large.D1=7,
    notch.large.D2=8,
    offset.small.D1=9,
    offset.small.D2=10,
    offset.large.D1=11,
    offset.large.D2=12,
    stringsAsFactors=FALSE
  )
  filter.params2 <- data.frame(
    quantile=2.5,
    beads.fsc.small=10,
    beads.D1=20,
    beads.D2=30,
    width=40,
    notch.small.D1=50,
    notch.small.D2=60,
    notch.large.D1=70,
    notch.large.D2=80,
    offset.small.D1=90,
    offset.small.D2=100,
    offset.large.D1=110,
    offset.large.D2=120,
    stringsAsFactors=FALSE
  )

  id1 <- save.filter.params(x$db, filter.params1)
  Sys.sleep(2)  # filter timestamp has resolution of seconds
  id2 <- save.filter.params(x$db, filter.params2)

  latest <- get.filter.params.latest(x$db)
  oldest <- get.filter.params.by.id(x$db, id1)

  # Skip id and date when comparing
  expect_equal(oldest[, 3:ncol(oldest)], filter.params1)
  expect_equal(latest[, 3:ncol(latest)], filter.params2)

  tearDown(x)
})

test_that("Retrieve OPP stats by file", {
  x <- setUp()

  opp <- get.opp.stats.by.file(x$db.full, "2014_185/2014-07-04T00-00-02+00-00")
  expect_equal(nrow(opp), 3)
  expect_equal(unique(opp$file), "2014_185/2014-07-04T00-00-02+00-00")

  tearDown(x)
})

test_that("Retrieve VCT stats by file", {
  x <- setUp()

  vct <- get.vct.stats.by.file(x$db.full, "2014_185/2014-07-04T00-03-02+00-00")
  expect_equal(nrow(vct), 12)
  expect_equal(unique(vct$file), "2014_185/2014-07-04T00-03-02+00-00")

  tearDown(x)
})

test_that("Retrieve OPP stats by date", {
  x <- setUp()

  opp <- get.opp.stats.by.date(x$db.full, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(
    opp$file,
    c(rep("2014_185/2014-07-04T00-00-02+00-00", 3), rep("2014_185/2014-07-04T00-03-02+00-00", 3))
  )

  opp <- get.opp.stats.by.date(x$db.full, "2014-07-04 00:03", "2014-07-04 00:17")
  expect_equal(opp$file, rep("2014_185/2014-07-04T00-03-02+00-00", 3))

  tearDown(x)
})

test_that("Retrieve VCT stats by date", {
  x <- setUp()

  vct <- get.vct.stats.by.date(x$db.full, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(
    vct$file,
    c(rep("2014_185/2014-07-04T00-00-02+00-00", 12), rep("2014_185/2014-07-04T00-03-02+00-00", 12))
  )

  vct <- get.vct.stats.by.date(x$db.full, "2014-07-04 0:03", "2014-07-04 00:17")
  expect_equal(
    vct$file,
    c(rep("2014_185/2014-07-04T00-03-02+00-00", 12))
  )

  tearDown(x)
})

test_that("Retrieve OPP by file", {
  x <- setUp()

  # Without VCT, transformed
  opp <- get.opp.by.file(x$opp.input.dir, get.opp.files(x$db.full)[1:2], 50)
  expect_equal(nrow(opp), 289)
  expect_true(!any(max(opp[, length(popcycle:::EVT.HEADER)]) > 10^3.5))

  # Without VCT, not transformed
  opp <- get.opp.by.file(x$opp.input.dir, get.opp.files(x$db.full)[1:2], 50, transform=F)
  expect_equal(nrow(opp), 289)
  expect_true(any(max(opp[, seq(3, length(popcycle:::EVT.HEADER))]) > 10^3.5))

  # With VCT
  opp <- get.opp.by.file(x$opp.input.dir, get.opp.files(x$db.full)[1:2], 50, vct.dir=x$vct.input.dir)
  expect_true("pop" %in% names(opp))

  # All quantiles
  opp <- get.opp.by.file(x$opp.input.dir, get.opp.files(x$db.full)[1], transform=F)
  expect_equal(nrow(opp), 427)

  tearDown(x)
})

test_that("Retrieve OPP by date", {
  x <- setUp()

  # Without VCT, transformed
  opp <- get.opp.by.date(x$db.full, x$opp.input.dir, 50, "2014-07-04 00:00", "2014-07-04 00:04")
  expect_equal(nrow(opp), 289)
  expect_true(!any(max(opp[, length(popcycle:::EVT.HEADER)]) > 10^3.5))

  # Without VCT, transformed
  opp <- get.opp.by.date(x$db.full, x$opp.input.dir, 50, "2014-07-04 00:03", "2014-07-04 00:17")
  expect_equal(nrow(opp), 182)
  expect_true(!any(max(opp[, length(popcycle:::EVT.HEADER)]) > 10^3.5))

  # Without VCT, not transformed
  opp <- get.opp.by.date(x$db.full, x$opp.input.dir, 50, "2014-07-04 00:00", "2014-07-04 00:04",
                         transform=F)
  expect_equal(nrow(opp), 289)
  expect_true(any(max(opp[, seq(3, length(popcycle:::EVT.HEADER))]) > 10^3.5))

  # With VCT
  opp <- get.opp.by.date(x$db.full, x$opp.input.dir, 50, "2014-07-04 00:00", "2014-07-04 00:04",
                         vct.dir=x$vct.input.dir)
  expect_true("pop" %in% names(opp))

  tearDown(x)
})

test_that("Retrieve split-quantile OPP file", {
  x <- setUp()

  # First get a multi-quantile OPP file
  opp_file <- get.opp.files(x$db.full)[1]
  opp <- get.opp.by.file(x$opp.input.dir, opp_file, transform=F)
  expect_equal(nrow(opp), 427)
  opp50 <- opp[opp["q50"] == TRUE, popcycle:::EVT.HEADER]

  # Save it manually as an old style split-quantile file
  opp_dir <- file.path(x$tmp.dir, "splitopp")
  out_path <- paste0(file.path(opp_dir, "50", opp_file), ".opp.gz")
  dir.create(dirname(out_path), showWarnings=F, recursive=T)
  writeSeaflow(opp50, out_path, untransform=F)

  # Read it back and make sure it's the same
  opp_again <- get.opp.by.file(opp_dir, opp_file, quantile=50, transform=F)
  rownames(opp50) <- NULL  # reset row names to match opp_again
  expect_true(identical(opp50, opp_again))

  tearDown(x)
})

test_that("Retrieve EVT file names by date", {
  x <- setUp()

  evt.files <- get.evt.files.by.date(x$db.full, x$evt.input.dir, "2014-07-04 00:03", "2014-07-04 00:10")
  answer <- c(
    "2014_185/2014-07-04T00-03-02+00-00",
    "2014_185/2014-07-04T00-06-02+00-00",
    "2014_185/2014-07-04T00-09-02+00-00"
  )
  expect_equal(evt.files, answer)

  tearDown(x)
})

test_that("Copy tables from one db to another", {
  x <- setUp()

  # To test we'll copy vct table from full db to bare db
  metadata_df <- data.frame(
    cruise=c("testcruisenew"),
    inst=c(100),
    stringsAsFactors=FALSE
  )
  reset.table(x$db.bare, "metadata")
  sql.dbWriteTable(x$db.bare, "metadata", metadata_df)
  popcycle:::copy_tables(x$db.full, x$db.bare, c("vct", "metadata"))
  src_vct <- get.vct.table(x$db.full)
  dest_vct <- get.vct.table(x$db.bare)
  src_metadata <- get.meta.table(x$db.full)
  dest_metadata <- get.meta.table(x$db.bare)
  expect_equal(src_vct, dest_vct)
  expect_equal(src_metadata, dest_metadata)

  tearDown(x)
})


test_that("Find common dbs in two directories", {
  x <- setUp()

  dir_a <- file.path(x$tmp.dir, "a", "suba")
  dir_b <- file.path(x$tmp.dir, "b", "subb")
  dir.create(dir_a, recursive=T)
  dir.create(dir_b, recursive=T)
  make.popcycle.db(file.path(dir_a, "w.db"))
  make.popcycle.db(file.path(dir_a, "x.db"))
  make.popcycle.db(file.path(dir_b, "y.db"))
  make.popcycle.db(file.path(dir_b, "x.db"))

  # One db in common
  common <- popcycle:::find_common_dbs(dir_a, dir_b)
  answer <- data.frame(
    basename="x.db",
    old_path=file.path(dir_a, "x.db"),
    new_path=file.path(dir_b, "x.db"),
    stringsAsFactors=FALSE
  )
  expect_equal(common, answer)

  # No db in common
  file.remove(file.path(dir_a, "x.db"))
  common <- popcycle:::find_common_dbs(dir_a, dir_b)
  expect_equal(nrow(common), 0)

  tearDown(x)
})

test_that("Get list of OPP files", {
  x <- setUp()

  # Set an outlier
  save.outliers(x$db.full, data.frame(file="2014_185/2014-07-04T00-00-02+00-00", flag=1))

  expect_equal(
    c("2014_185/2014-07-04T00-00-02+00-00", "2014_185/2014-07-04T00-03-02+00-00"),
    get.opp.files(x$db.full, outliers=F)
  )

  expect_equal(
    c("2014_185/2014-07-04T00-03-02+00-00"),
    get.opp.files(x$db.full, outliers=T)
  )

  expect_equal(
    7,
    length(get.opp.files(x$db.full, outliers=F, all.files=T))
  )

  expect_equal(
    6,
    length(get.opp.files(x$db.full, outliers=T, all.files=T))
  )
})
