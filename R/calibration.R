#' Calculation of flow rate based on stream pressure measured by SeaFlow and the ratio of the detectable region
#'
#' @param  a vector that contains stream pressure measured by SeaFlow (available in SFL table and STAT table).
#' @param inst Instrument serial. If not provided this will attempt to be
#'   parsed from the SFL file name (<cruise>_<serial>.sfl).
#' @return A dataframe with flow rate estimates
#' @export
flowrate <- function(stream_pressure, inst=inst){

  load(system.file("flowrate", paste0("lm_",inst),package="popcycle"))
    fr <- predict(reg, newdata=data.frame(measured.pressure=log10(stream_pressure)),interval="predict")

    # ratio of the volume of the stream analyzed by the laser (aka, detectable region) to the whole water stream (200 µm nozzle) for that instrument
    drr <- read.csv(system.file("flowrate", "detectable_region.csv",package="popcycle"))
    drr.mean <- mean(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])
    drr.sd <- sd(drr[which(drr$seaflow_serial == inst), "detectable_region_ratio"])

    flow_rate <- drr.mean * 10^fr[,"fit"]  # mL min-1
    flow_rate.sd <- flow_rate * sqrt((log(10) * matrixStats::rowSds(fr))^2 + (drr.sd /drr.mean)^2) # uncertainties Antilog, base 10 : y=10^a so Sy= log(10) * y * Sa

    return(data.frame(cbind(flow_rate, flow_rate.sd)))
}




#' Estimate cell diameter and carbon cell quotas and biomass of phytoplankton populations based on normalized forward scatter to 1-µm beads used as internal standard
#'
#' By default, forward scatter normalized to 1-µm beads is converted to
#' equivalent spherical diameter (µm) and carbon quota (pgC cell-1) with the
#' polynomials of the Mie-based calibration
#' (https://github.com/seaflow-uw/seaflow-fsc-calibration), see
#' \code{\link{read_scatter_calib_csv}}. Normalized scatter outside the
#' calibrated range is clamped to the range limits. If a Mie theory lookup
#' table is provided with \code{mie}, the legacy lookup-table conversion is used
#' instead.
#'
#' @param opp Table that contains fsc_small values (transformed data).
#' @param beads_fsc Values of the fsc_small for beads (transformed data).
#' @param inst Instrument serial number
#' @param mie Optional legacy Mie theory lookup dataframe (see
#'   \code{\link{read_mie_csv}}). If provided, it is used instead of the
#'   polynomial calibration.
#' @param calib Optional polynomial calibration dataframe. If NULL, the
#'   installed table returned by \code{\link{read_scatter_calib_csv}} is used.
#' @return A dataframe with cell size and carbon cell quotas
#' @export
size_carbon_conversion <- function(opp, beads_fsc, inst, mie = NULL, calib = NULL){
    was_tibble <- FALSE
    if ("tbl_df" %in% class(opp)) {
      opp <- as.data.frame(opp)
    }
    if (is.null(mie)) {
      return(size_carbon_conversion_poly(opp, beads_fsc, inst, calib))
    }

    # Legacy conversion: find closest matches in Mie lookup table
    id <- findInterval(opp[,"fsc_small"]/as.numeric(beads_fsc), mie$scatter, all.inside = TRUE)

    #convert scatter to diameter and Qc
    for (quant in c("_lwr", "_mid", "_upr")) {
      diams <- mie[id, paste0("diam_", inst, quant)]
      if (is.null(diams)) {
        stop(paste0("missing entry in mie table for ", paste0("diam_", inst, quant)))
      }
      opp[, paste0("diam", quant)] <- diams

      qcs <- mie[id, paste0("Qc_", inst, quant)]
      if (is.null(qcs)) {
        stop(paste0("missing entry in mie table for ", paste0("Qc_", inst, quant)))
      }
      opp[, paste0("Qc", quant)] <- qcs
    }

    if (was_tibble) {
      opp <- tibble::as_tibble(opp)
    }

  return(opp)
}

# Polynomial conversion of normalized forward scatter to diameter and Qc.
# For each refractive index ("lwr", "mid", "upr"):
#   log10(y) = sum_k a_k * log10(s)^k
# with s = fsc_small / beads_fsc clamped to [scatter_min, scatter_max].
# Adds columns in the same order as the legacy lookup-table conversion:
# diam_lwr, Qc_lwr, diam_mid, Qc_mid, diam_upr, Qc_upr.
size_carbon_conversion_poly <- function(opp, beads_fsc, inst, calib = NULL) {
  if (is.null(calib)) {
    calib <- read_scatter_calib_csv()
  }
  inst <- as.character(inst)
  scatter <- opp[, "fsc_small"] / as.numeric(beads_fsc)

  for (quant in c("lwr", "mid", "upr")) {
    for (variable in c("diameter", "Qc")) {
      row <- calib[as.character(calib$instrument) == inst &
                     calib$index == quant &
                     calib$variable == variable, ]
      colname <- paste0(ifelse(variable == "diameter", "diam", "Qc"), "_", quant)
      if (nrow(row) != 1) {
        stop(paste0("missing entry in scatter calibration table for instrument ",
                    inst, ", ", colname))
      }
      coefs <- as.numeric(row[1, grep("^a[0-9]+$", names(row))])
      s <- pmin(pmax(scatter, row$scatter_min), row$scatter_max)
      opp[, colname] <- 10^eval_polynomial(coefs, log10(s))
    }
  }

  return(opp)
}

# Evaluate a polynomial with coefficients in increasing order (a0, a1, ...)
# at x using Horner's method. Vectorized over x.
eval_polynomial <- function(coefs, x) {
  y <- rep(coefs[length(coefs)], length(x))
  for (k in rev(seq_len(length(coefs) - 1))) {
    y <- y * x + coefs[k]
  }
  y[is.na(x)] <- NA
  return(y)
}

#' Calibrate abundance based on Influx data
#'
#' @param stat The stat table returned from get_stat_table()
#' @param cruisename Name of the cruise (returned from get_metadata_table()).
#' @param calib Optional calibration calibration  dataframe of regression values (cruise=cruise.name, pop=prochloro, a=slope, b=intercept)
#' @return the stat table witrh corrected abundancce if available
#' @export
stat_calibration <- function(stat, cruisename, calib=NULL){

    if (is.null(calib)) {
        calib <- read_calib_csv()
    }

  popname <- unique(calib$pop)
  for(phyto in popname){
    corr <- calib %>% dplyr::filter(cruise == cruisename & pop == phyto)
    if(nrow(corr) > 0){ 
      print(paste("Calibrated abundance for", phyto, ": a =", corr$a))
      id <- which(stat$pop == phyto)
      stat[id,c("abundance")]  <- stat[id,c("abundance")] * corr$a  # cells µL-1
    }
  }  
  
  return(stat)
}