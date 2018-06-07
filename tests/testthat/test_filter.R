context("EVT filtering")
library(popcycle)

test_that("Filter EVT files", {
  x <- setUp()

  save.sfl(x$db, sfl.file=x$sfl.file, cruise=x$cruise, inst=x$serial)

  inflection <- data.frame(fsc=c(33759), d1=c(19543), d2=c(19440))
  filter.params <- create.filter.params(x$serial, inflection$fsc, inflection$d1,
                                        inflection$d2, x$slope.file)

  filter.id <- save.filter.params(x$db, filter.params)

  evt.files <- get.evt.files(x$evt.input.dir)

  expect_warning(filter.evt.files(x$db, x$evt.input.dir, evt.files, x$opp.dir))
  filter.params <- get.filter.params.latest(x$db)
  opp.stats <- get.opp.table(x$db)

  expect_equal(nrow(opp.stats), 6)  # 2 files, 3 quantiles each

  # Sort by file then quantile
  opp.stats <- opp.stats[order(opp.stats$file, opp.stats$quantile), ]

  expect_equal(opp.stats[, "filter_id"], rep(filter.id, 6))
  expect_equal(unique(opp.stats[, "file"]), c("2014_185/2014-07-04T00-00-02+00-00",
                                              "2014_185/2014-07-04T00-03-02+00-00"))
  expect_equal(unique(opp.stats[, "all_count"]), 40000)
  expect_equal(unique(opp.stats[, "evt_count"]), c(39928, 39925))
  expect_equal(opp.stats[, "opp_count"], c(423, 107, 86, 492, 182, 147))
  expect_equal(opp.stats[, "opp_evt_ratio"], c(0.010594069, 0.002679824, 0.002153877, 0.012323106, 0.004558547, 0.003681904))
  expect_equal(opp.stats[, "quantile"], c(2.5, 50, 97.5, 2.5, 50, 97.5))

  # Make sure written OPP files have the same number of particles as those
  # recorded in database opp table
  opp.files <- get.opp.files(x$db)
  i <- 1
  for (opp.file in opp.files) {
    for (q in c(2.5, 50, 97.5)) {
      opp <- get.opp.by.file(x$opp.dir, opp.file, q)
      expect_equal(opp.stats[opp.stats$file == opp.file & opp.stats$quantile == q, "opp_count"], nrow(opp))
    }
  }

  tearDown(x)
})

test_that("Create filter parameters", {
  x <- setUp()

  inflection <- data.frame(fsc=c(33759), d1=c(19543), d2=c(19440))
  filter.params <- create.filter.params(x$serial, inflection$fsc, inflection$d1,
                                        inflection$d2, x$slope.file)

  headers <- c("quantile", "beads.fsc.small", "beads.D1",
               "beads.D2", "width", "notch.small.D1", "notch.small.D2",
               "notch.large.D1", "notch.large.D2", "offset.small.D1",
               "offset.small.D2", "offset.large.D1", "offset.large.D2")
  a <- data.frame(matrix(ncol = 14, nrow = 0))
  names(a) <- headers

  b <- data.frame(2.5, inflection$fsc, inflection$d1, inflection$d2,
                  2500, 0.614, 0.651, 1.183, 1.208, 1418, 1080, -17791, -17724)
  names(b) <- headers
  a <- rbind(a, b)

  b <- data.frame(50.0, inflection$fsc, inflection$d1, inflection$d2,
                  2500, 0.656, 0.683, 1.635, 1.632, 0, 0, -33050, -32038)
  names(b) <- headers
  a <- rbind(a, b)

  b <- data.frame(97.5, inflection$fsc, inflection$d1, inflection$d2,
                  2500, 0.698, 0.714, 2.087, 2.056, -1418, -1047, -48309, -46352)
  names(b) <- headers
  a <- rbind(a, b)

  expect_equal(filter.params, a)

  tearDown(x)
})
