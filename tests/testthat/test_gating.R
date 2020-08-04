context("Gating operations")
library(popcycle)

source("helper.R")

test_that("Classify one file", {
  x <- setUp()

  file_id <- "2014_185/2014-07-04T00-00-02+00-00"
  vct_dir <- file.path(x$tmp.dir, "vct")
  reset.vct.stats.table(x$db.full)
  if (nrow(get.vct.table(x$db.full)) > 0) {  # make sure we deleted it
    stop("vct table not properly deleted")
  }
  mie <- read_mie_csv("../testdata/calibrated-mie.de77773.2020-10-23.csv")
  classify.opp.files(x$db.full, x$opp.input.dir, file_id, vct_dir, mie.table=mie)
  # Uncomment this line to capture test output VCT
  #file.copy(file.path(vct_dir, "2014-07-04T00-00-00+00-00.1H.vct.parquet"), "~/Desktop/")

  # Expected contents
  gating_id <- "0e283d1c-ba99-4dca-99bb-35735eeafc48"
  opp_expected <- tibble::as_tibble(
    arrow::read_parquet(file.path(x$opp.input.dir, "2014-07-04T00-00-00+00-00.1H.opp.parquet"))
  )
  vct_expected <- tibble::as_tibble(arrow::read_parquet("../testdata/vct/2014-07-04T00-00-00+00-00.1H.vct.parquet"))
  vct_stats_expected <- get.vct.stats.by.file(x$db.full, file_id)

  # These lines get expected contents from last version of popcycle with CSV VCT
  # output format.
  # con <- gzfile(description=file.path(x$vct.input.dir, "2.5", paste0(file_id, ".vct.gz")))
  # cols25 <- c("diam_lwr_q2.5", "Qc_lwr_q2.5", "diam_mid_q2.5", "Qc_mid_q2.5", "diam_upr_q2.5", "Qc_upr_q2.5", "pop_q2.5")
  # vct25_expected <- tibble::as_tibble(read.table(con, col.names=cols25, stringsAsFactors=T))
  # con <- gzfile(description=file.path(x$vct.input.dir, "50", paste0(file_id, ".vct.gz")))
  # cols50 <- c("diam_lwr_q50", "Qc_lwr_q50", "diam_mid_q50", "Qc_mid_q50", "diam_upr_q50", "Qc_upr_q50", "pop_q50")
  # vct50_expected <- tibble::as_tibble(read.table(con, col.names=cols50, stringsAsFactors=T))
  # con <- gzfile(description=file.path(x$vct.input.dir, "97.5", paste0(file_id, ".vct.gz")))
  # cols975 <- c("diam_lwr_q97.5", "Qc_lwr_q97.5", "diam_mid_q97.5", "Qc_mid_q97.5", "diam_upr_q97.5", "Qc_upr_q97.5", "pop_q97.5")
  # vct975_expected <- tibble::as_tibble(read.table(con, col.names=cols975, stringsAsFactors=T))

  # Test VCT file contents
  vct_out_path <- file.path(vct_dir, "2014-07-04T00-00-00+00-00.1H.vct.parquet")
  vct_got <- tibble::as_tibble(arrow::read_parquet(vct_out_path))
  expect_equal(vct_got, vct_expected)
  expect_equal(unique(levels(vct_got$gating_id)), gating_id)
  # Test against last version of popcycle with CSV VCT
  # expect_equal(vct_got[vct_got$q2.5 & vct_got$file_id == file_id, cols25], vct25_expected)
  # expect_equal(vct_got[vct_got$q50 & vct_got$file_id == file_id, cols50], vct50_expected)
  # expect_equal(vct_got[vct_got$q97.5 & vct_got$file_id == file_id, cols975], vct975_expected)

  # Test VCT stats
  vct_stats_got <- get.vct.stats.by.file(x$db.full, file_id)
  expect_equal(vct_stats_got, vct_stats_expected)
  expect_equal(unique(vct_stats_got$file), "2014_185/2014-07-04T00-00-02+00-00")

  # Test OPP contents of VCT file
  opp_cols <- c("date", "file_id", "D1", "D2", "fsc_small", "pe", "chl_small", "filter_id")
  expect_equal(
    opp_expected[opp_expected$q2.5 & (opp_expected$file_id == file_id), opp_cols],
    vct_got[vct_got$q2.5 & (vct_got$file_id == file_id), opp_cols]
  )
  expect_equal(
    opp_expected[opp_expected$q50 & (opp_expected$file_id == file_id), opp_cols],
    vct_got[vct_got$q50 & (vct_got$file_id == file_id), opp_cols]
  )
  expect_equal(
    opp_expected[opp_expected$q97.5 & (opp_expected$file_id == file_id), opp_cols],
    vct_got[vct_got$q97.5 & (vct_got$file_id == file_id), opp_cols]
  )

  tearDown(x)
})
