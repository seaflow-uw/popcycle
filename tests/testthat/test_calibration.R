context("Scatter to diameter and carbon quota conversion")
library(popcycle)

source("helper.R")

INSTS <- c("740", "751", "989", "130")

# Build an OPP-like data frame whose normalized scatter (fsc_small / beads_fsc)
# equals s
opp_with_scatter <- function(s, beads_fsc = 1000) {
  data.frame(fsc_small = s * beads_fsc, pe = rep(1, length(s)), chl_small = rep(1, length(s)))
}

test_that("Polynomial calibration table covers all instruments", {
  calib <- read_scatter_calib_csv()
  expect_true(all(INSTS %in% calib$instrument))
  for (inst in INSTS) {
    for (variable in c("diameter", "Qc")) {
      for (quant in c("lwr", "mid", "upr")) {
        n <- sum(calib$instrument == inst & calib$variable == variable & calib$index == quant)
        expect_equal(n, 1)
      }
    }
  }
})

test_that("Polynomial conversion matches reference values", {
  # Reference values computed independently with R/apply_calibration.R in
  # https://github.com/seaflow-uw/seaflow-fsc-calibration (commit ed7a84c)
  s <- c(0.0005, 0.01, 0.1, 1, 5, 20)
  expected <- list(
    "740" = list(diam = c(0.3076011943, 0.6496821551, 1.090913177, 2.703396758, 6.447609308, 14.47600652),
                 Qc = c(0.007144772544, 0.04917538207, 0.187274224, 1.946733835, 18.33276636, 147.724711)),
    "751" = list(diam = c(0.2285570641, 0.5467273727, 1.003883608, 2.960036045, 8.309431802, 21.43011496),
                 Qc = c(0.00332036262, 0.03150852209, 0.1511201581, 2.459946044, 35.27551381, 406.4644193)),
    "989" = list(diam = c(0.2551454432, 0.5836668573, 1.03754634, 2.879587292, 7.615598681, 18.55786512),
                 Qc = c(0.004410541257, 0.03729796252, 0.1645427228, 2.291138605, 28.16928161, 280.4023173)),
    "130" = list(diam = c(0.307148767, 0.654326726, 1.112238443, 2.836972482, 6.895431663, 15.50307212),
                 Qc = c(0.007117691576, 0.05008752173, 0.1968656339, 2.204679889, 21.80057187, 176.3019688))
  )
  for (inst in INSTS) {
    got <- size_carbon_conversion(opp_with_scatter(s), beads_fsc = 1000, inst = inst)
    expect_equal(got$diam_mid, expected[[inst]]$diam, tolerance = 1e-8)
    expect_equal(got$Qc_mid, expected[[inst]]$Qc, tolerance = 1e-8)
  }
})

test_that("Polynomial conversion adds the same columns as the legacy lookup", {
  got <- size_carbon_conversion(opp_with_scatter(c(0.01, 1)), beads_fsc = 1000, inst = "740")
  expect_equal(names(got), c("fsc_small", "pe", "chl_small",
                             "diam_lwr", "Qc_lwr", "diam_mid", "Qc_mid", "diam_upr", "Qc_upr"))
  # Tibble input is accepted
  got_tbl <- size_carbon_conversion(tibble::as_tibble(opp_with_scatter(c(0.01, 1))),
                                    beads_fsc = 1000, inst = "740")
  expect_equal(got_tbl$diam_mid, got$diam_mid)
  # Numeric instrument serial is accepted
  got_num <- size_carbon_conversion(opp_with_scatter(c(0.01, 1)), beads_fsc = 1000, inst = 740)
  expect_equal(got_num$diam_mid, got$diam_mid)
})

test_that("Diameters and Qc are ordered, monotonic and consistent", {
  s <- 10^seq(log10(0.0005), log10(20), length.out = 2000)
  for (inst in INSTS) {
    got <- size_carbon_conversion(opp_with_scatter(s), beads_fsc = 1000, inst = inst)
    # lower refractive index -> larger cells
    expect_true(all(got$diam_lwr <= got$diam_mid))
    expect_true(all(got$diam_mid <= got$diam_upr))
    expect_true(all(diff(got$diam_mid) > 0))
    # Qc = 0.261 * V^0.860 (Menden-Deuer & Lessard 2000) applied to diameter
    for (quant in c("lwr", "mid", "upr")) {
      d <- got[[paste0("diam_", quant)]]
      expect_equal(got[[paste0("Qc_", quant)]], 0.261 * (pi / 6 * d^3)^0.860, tolerance = 1e-8)
    }
  }
})

test_that("Normalized scatter outside the calibrated range is clamped", {
  got <- size_carbon_conversion(opp_with_scatter(c(1e-6, 0.0005, 20, 1000)),
                                beads_fsc = 1000, inst = "751")
  expect_equal(got$diam_mid[1], got$diam_mid[2])
  expect_equal(got$diam_mid[4], got$diam_mid[3])
  expect_equal(got$Qc_upr[1], got$Qc_upr[2])
  expect_equal(got$Qc_upr[4], got$Qc_upr[3])
  # Zero scatter is clamped too, not -Inf
  got0 <- size_carbon_conversion(opp_with_scatter(0), beads_fsc = 1000, inst = "751")
  expect_equal(got0$diam_mid, got$diam_mid[2])
})

test_that("Missing values and empty input are handled", {
  got <- size_carbon_conversion(opp_with_scatter(c(NA, 1)), beads_fsc = 1000, inst = "740")
  expect_true(is.na(got$diam_mid[1]))
  expect_false(is.na(got$diam_mid[2]))
  got_empty <- size_carbon_conversion(opp_with_scatter(numeric(0)), beads_fsc = 1000, inst = "740")
  expect_equal(nrow(got_empty), 0)
  expect_true("Qc_upr" %in% names(got_empty))
})

test_that("Unknown instrument is an error", {
  expect_error(
    size_carbon_conversion(opp_with_scatter(1), beads_fsc = 1000, inst = "999"),
    "missing entry in scatter calibration table"
  )
})

test_that("Legacy Mie lookup table is still used when provided", {
  mie <- read_mie_csv("../testdata/calibrated-mie.de77773.2020-10-23.csv")
  s <- c(0.001, 0.05, 0.5, 2, 10)
  got <- size_carbon_conversion(opp_with_scatter(s), beads_fsc = 1000, inst = "740", mie = mie)
  id <- findInterval(s, mie$scatter, all.inside = TRUE)
  expect_equal(got$diam_mid, mie[id, "diam_740_mid"])
  expect_equal(got$Qc_lwr, mie[id, "Qc_740_lwr"])
})

test_that("Classify all files with the polynomial calibration", {
  x <- setUp()

  file.copy(x$db.full.one, file.path(x$tmp.dir, "test.db"))
  db <- file.path(x$tmp.dir, "test.db")
  reset_vct_table(db)
  gating_id <- "0e283d1c-ba99-4dca-99bb-35735eeafc48"
  # Gating plan (bead values and serial per file), built before classification
  # because the plan skips files already classified with this gating ID
  plan <- popcycle:::create_full_gating_plan(
    NULL, db, normalizePath(x$opp.one.input.dir), x$vct.output.dir,
    gating_id = gating_id
  )
  expect_true(nrow(plan) > 0)
  # No mie_table: default polynomial calibration
  classify_opp_files(db, x$opp.one.input.dir, NULL, x$vct.output.dir,
                     gating_id = gating_id)

  for (f in c("2014-07-04T00-00-00+00-00.1H.vct.parquet",
              "2014-07-04T01-00-00+00-00.1H.vct.parquet")) {
    expected <- arrow::read_parquet(file.path(x$vct.one.input.dir, f))  # legacy lookup
    got <- arrow::read_parquet(file.path(x$vct.output.dir, f))
    # Same rows, columns and population assignments: gating does not depend
    # on the size conversion
    expect_equal(dim(got), dim(expected))
    expect_equal(names(got), names(expected))
    pop_cols <- grep("^pop_q", names(got), value = TRUE)
    expect_equal(got[, pop_cols], expected[, pop_cols])
    # Size and carbon come from the polynomial calibration
    for (col in grep("^(diam|Qc)_", names(got), value = TRUE)) {
      v <- got[[col]]
      expect_true(all(is.na(v) | (is.finite(v) & v > 0)))
    }
    expect_false(isTRUE(all.equal(got$diam_mid_q50, expected$diam_mid_q50)))
  }

  # Stored VCT values equal a direct conversion of each particle's fsc_small
  # with the bead value used for that file and quantile
  vct <- dplyr::bind_rows(lapply(
    list.files(x$vct.output.dir, pattern = "\\.vct\\.parquet$", full.names = TRUE),
    arrow::read_parquet
  ))
  n_checked <- 0
  for (fid in unique(as.character(vct$file_id))) {
    p <- plan[plan$file_id == fid, ]
    for (quant in popcycle:::QUANTILES) {
      rows <- vct[as.character(vct$file_id) == fid & vct[[paste0("q", quant)]], ]
      if (nrow(rows) == 0) next
      direct <- size_carbon_conversion(
        as.data.frame(rows[, "fsc_small"]),
        beads_fsc = p[[paste0("beads_fsc_", quant)]][1], inst = p$inst[1]
      )
      expect_equal(rows[[paste0("diam_mid_q", quant)]], direct$diam_mid)
      expect_equal(rows[[paste0("Qc_upr_q", quant)]], direct$Qc_upr)
      n_checked <- n_checked + nrow(rows)
    }
  }
  expect_true(n_checked > 0)

  # VCT summary statistics are computed from the new values
  vct_stats <- get_vct_table(db)
  expect_true(nrow(vct_stats) > 0)
  expected_stats <- get_vct_table(x$db.full.one)
  expect_equal(nrow(vct_stats), nrow(expected_stats))
  expect_equal(vct_stats$count, expected_stats$count)
  expect_true(all(is.finite(vct_stats$diam_mid_med)))
  expect_true(all(is.finite(vct_stats$Qc_mid_mean)))

  tearDown(x)
})
