context("DB operations")
library(popcycle)

source("helper.R")

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

  id1 <- save_filter_params(x$db, filter.params1)
  Sys.sleep(2)  # filter timestamp has resolution of seconds
  id2 <- save_filter_params(x$db, filter.params2)

  oldest <- get_filter_params_by_id(x$db, id1)
  latest <- get_filter_params_by_id(x$db, id2)

  # Skip id and date when comparing
  expect_equal(oldest[, 3:ncol(oldest)], filter.params1)
  expect_equal(latest[, 3:ncol(latest)], filter.params2)

  tearDown(x)
})

test_that("Copy tables from one db to another", {
  x <- setUp()

  # To test we'll copy vct, metadata, outlier tables from full db to bare db
  metadata_df <- data.frame(
    cruise=c("testcruisenew"),
    inst=c(100),
    stringsAsFactors=FALSE
  )
  reset_metadata_table(x$db.bare)
  save_metadata(x$db.bare, metadata_df)

  outliers_df <- data.frame(
    file_id=c("2014_185/2014-07-04T00-03-02+00-00", "2014_185/2014-07-04T00-09-02+00-00"),
    flag=c(1, 3),
    stringsAsFactors=FALSE
  )
  save_outliers(x$db.full.one, outliers_df)
  # Need to initialize outlier table in dest db (bare)
  outliers_df <- data.frame(
    file_id=get_outlier_table(x$db.full.one)$file_id,
    flag=0,
    stringsAsFactors=FALSE
  )
  save_outliers(x$db.bare, outliers_df)

  popcycle:::copy_tables(x$db.full.one, x$db.bare, c("vct", "metadata"))
  popcycle:::copy_outlier_table(x$db.full.one, x$db.bare)
  src_vct <- get_vct_table(x$db.full.one, sfl_join = FALSE, outlier_join = FALSE)
  dest_vct <- get_vct_table(x$db.bare, sfl_join = FALSE, outlier_join = FALSE)
  src_metadata <- get_metadata_table(x$db.full.one)
  dest_metadata <- get_metadata_table(x$db.bare)
  src_outliers <- get_outlier_table(x$db.full.one)
  dest_outliers <- get_outlier_table(x$db.bare)
  expect_equal(dest_vct, src_vct)
  expect_equal(dest_metadata, src_metadata)
  expect_equal(dest_outliers, src_outliers)
  tearDown(x)
})


test_that("Find common dbs in two directories", {
  x <- setUp()

  dir_a <- file.path(x$tmp.dir, "a", "suba")
  dir_b <- file.path(x$tmp.dir, "b", "subb")
  dir.create(dir_a, recursive=T)
  dir.create(dir_b, recursive=T)
  make_popcycle_db(file.path(dir_a, "w.db"))
  make_popcycle_db(file.path(dir_a, "x.db"))
  make_popcycle_db(file.path(dir_b, "y.db"))
  make_popcycle_db(file.path(dir_b, "x.db"))

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

test_that("Save metadata", {
  x <- setUp()

  new_meta <- tibble::tibble(cruise = "newcruise", inst = "999")
  save_metadata(x$db.bare, new_meta)
  meta2 <- get_metadata_table(x$db.bare)
  expect_equal(as.data.frame(meta2), as.data.frame(new_meta))
})

test_that("Save outliers, overwrite", {
  x <- setUp()

  save_outliers(
    x$db.full.one,
    tibble::tibble(
      file_id = c(
        "2014_185/2014-07-04T00-00-02+00-00",
        "2014_185/2014-07-04T00-03-02+00-00",
        "2014_185/2014-07-04T02-03-02+00-00"
      ),
      flag = 1
    )
  )

  expect_equal(
    get_outlier_table(x$db.full.one),
    tibble::tibble(
      file_id = c(
        "2014_185/2014-07-04T00-00-02+00-00",
        "2014_185/2014-07-04T00-03-02+00-00",
        "2014_185/2014-07-04T00-06-02+00-00",
        "2014_185/2014-07-04T00-09-02+00-00",
        "2014_185/2014-07-04T00-12-02+00-00",
        "2014_185/2014-07-04T01-15-02+00-00",
        "2014_185/2014-07-04T01-17-02+00-00",
        "2014_185/2014-07-04T01-30-02+00-00",
        "2014_185/2014-07-04T02-03-02+00-00"
      ),
      flag = c(1, 1, 0, 0, 0, 0, 0, 0, 1)
    )
  )
})

test_that("Save outliers, don't overwrite", {
  x <- setUp()

  save_outliers(
    x$db.full.one,
    tibble::tibble(
      file_id = c(
        "2014_185/2014-07-04T00-00-02+00-00",
        "2014_185/2014-07-04T00-03-02+00-00",
        "2014_185/2014-07-04T02-03-02+00-00"
      ),
      flag = 1
    ),
    overwrite = FALSE
  )

  expect_equal(
    get_outlier_table(x$db.full.one),
    tibble::tibble(
      file_id = c(
        "2014_185/2014-07-04T00-00-02+00-00",
        "2014_185/2014-07-04T00-03-02+00-00",
        "2014_185/2014-07-04T00-06-02+00-00",
        "2014_185/2014-07-04T00-09-02+00-00",
        "2014_185/2014-07-04T00-12-02+00-00",
        "2014_185/2014-07-04T01-15-02+00-00",
        "2014_185/2014-07-04T01-17-02+00-00",
        "2014_185/2014-07-04T01-30-02+00-00",
        "2014_185/2014-07-04T02-03-02+00-00"
      ),
      flag = c(0, 0, 0, 0, 0, 0, 0, 0, 1)
    )
  )
})
