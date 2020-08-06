var_data <- c("time", "lat", "lon",
          "temp", "salinity", "par",
          "quantile", "pop",
          "chl_1q", "chl_med", "chl_3q",
          "pe_1q", "pe_med", "pe_3q",
          "fsc_1q", "fsc_med", "fsc_3q",
          "diam_lwr_1q", "diam_lwr_med", "diam_lwr_3q",
          "diam_mid_1q", "diam_mid_med", "diam_mid_3q",
          "diam_upr_1q", "diam_upr_med", "diam_upr_3q",
          "Qc_lwr_1q", "Qc_lwr_med", "Qc_lwr_mean", "Qc_lwr_3q",
          "Qc_mid_1q", "Qc_mid_med", "Qc_mid_mean", "Qc_mid_3q",
          "Qc_upr_1q", "Qc_upr_med", "Qc_upr_mean","Qc_upr_3q",
          "abundance","abundance_se",
          "flag")

var_long_name <- c("time of sample collection (UTC)",
                  "latitude",
                  "longitude",
                  "sea surface temperature",
                  "sea surface salinity",
                  "sea surface solar irradience",
                  "quantile",
                  "name of cytometric population",
                  "25% percentile of chlorophyll fluorescence",
                  "50% percentile of chlorophyll fluorescence",
                  "75% percentile of chlorophyll fluorescence",
                  "25% percentile of phycoerythrin fluorescence",
                  "50% percentile of phycoerythrin fluorescence",
                  "75% percentile of phycoerythrin fluorescence",
                  "25% percentile of forward scatter",
                  "50% percentile of the forward scatter",
                  "75% percentile of forward scatter",
                  "25% percentile of equivalent spherical diameter using high refractive index",
                  "50% percentile of equivalent spherical diameter using high refractive index",
                  "75% percentile of equivalent spherical diameter using high refractive index",
                  "25% percentile of equivalent spherical diameter using mid refractive index",
                  "50% percentile of equivalent spherical diameter using mid refractive index",
                  "75% percentile of equivalent spherical diameter using mid refractive index",
                  "25% percentile of equivalent spherical diameter using low refractive index",
                  "50% percentile of equivalent spherical diameter using low refractive index",
                  "75% percentile of equivalent spherical diameter using low refractive index",
                  "25% percentile of cellular carbon content using high refractive index",
                  "50% percentile of cellular carbon content using high refractive index",
                  "linear mean of cellular carbon content using high refractive index",
                  "75% percentile of cellular carbon content using high refractive index",
                  "25% percentile of cellular carbon content using mid refractive index",
                  "50% percentile of cellular carbon content using mid refractive index",
                  "linear mean of cellular carbon content using mid refractive index",
                  "75% percentile of cellular carbon content using mid refractive index",
                  "25% percentile of cellular carbon content using low refractive index",
                  "50% percentile of cellular carbon content using low refractive index",
                  "linear mean of cellular carbon content using low refractive index",
                  "75% percentile of cellular carbon content using low refractive index",
                  "cell abundance",
                  "standard error of cell abundance",
                  "outliers")

var_comment <-  c(rep('none',3),
                  rep('uncurated data broadcasted by the ship (as is)',3),
                  'interval confidence for OPP filtration (2.5 = permissive approach; 50 = standard approach; 97.5 = conservative approach); see https://github.com/seaflow-uw/seaflow-filter for more details',
                  'prochloro (Prochlorococcus) synecho (Synechococcus) picoeuk (picoeukaryote phytoplankton) beads (internal standard) croco (Crocosphaera-like particles) unknown (unclassified particles)',
                  rep('chlorophyll fluorescence (collected using a 692-40 bandpass filter)',3),
                  rep('phycoerythrin fluorescence (collected using a 572-27 bandpass filter)',3),
                  rep('forward angle light scatter (collected using a 457-50 bandpass filter)',3),
                  rep('cell diameter based on Mie theory using an index of refraction of 1.41 for phytoplankton and 1.337 for seawater, see https://github.com/seaflow-uw/fsc-size-calibration for more details',3),
                  rep('cell diameter based on Mie theory using an index of refraction of 1.38 for phytoplanktonand 1.337 for seawater, see https://github.com/seaflow-uw/fsc-size-calibration for more details',3),
                  rep('cell diameter based on Mie theory using an index of refraction of 1.36 for phytoplanktonand 1.337 for seawater, see https://github.com/seaflow-uw/fsc-size-calibration for more details',3),
                  rep('carbon content based on the equation fgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter (Mie-based, using refractive index of 1.41 for phytoplankton) assuming spherical particle; see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                  rep('carbon content based on the equation fgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter (Mie-based, using refractive index of 1.38 for phytoplankton) assuming spherical particle; see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                  rep('carbon content based on the equation fgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter (Mie-based, using refractive index of 1.35 for phytoplankton) assuming spherical particle; see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                  'cell abundance, see https://github.com/seaflow-uw/seaflow-virtualcore for more details',
                  'standard error of cell abundance based on uncertainties in converting sample stream pressure to flow rate, see https://github.com/seaflow-uw/seaflow-virtualcore for more details',
                  'outliers (0 = Quality data; 1 = issue related to instrument performance; 2 = issue related to OPP filtration; 3 = issue related to population classification)')

var_unit <- c("%Y-%m-%dT%H:%M:%S",
              "decimal degree North",
              "decimal degree East",
              "deg C",
              "psu",
              "micromol photons m-2 s-1",
              "percent",
              "unitless",
              rep("unitless",9),
              rep("micrometer",9),
              rep("picogram carbon per cell",12),
              rep("cells per microliter",2),
              "unitless")

var_sensor <- c("SeaFlow",
                rep("data broadcasted by ship", 2),
                rep("data broadcasted by ship", 3),
                rep("SeaFlow",35))

var_discipline <- c(rep("", 3),
                   rep("physics, abiotic", 3),
                   "optics, SeaFlow",
                   "taxonomy, cytometry, SeaFlow",
                   rep("optics, cytometry, SeaFlow",9),
                   rep("biology, biogeochemistry, optics, cytometry, SeaFlow",21),
                   rep("biology, biogeochemistry, SeaFlow",2),
                   "optics, SeaFlow")

visualize <- c(rep(0, 3), rep(1, 3), rep(0, 2),  rep(1, 11), 0)




#' Convert data from sqlite3 database into a csv file of curated SeaFlow data for a cruise, along with metadata files.
#'
#' @param db SQLite3 database file path.
#' @param meta SeaFlow instrument log file.
#' @param path Path to save the file.
#' @param version Version of the dataset.
#' @return None
#' @examples
#' \dontrun{
#' csv_convert(db, meta, path)
#' }
#' @export
csv_convert<- function(db, meta, path, version = "v1.3") {
   
    cruise <- sub(".db", "",basename(db))
    print(paste("formatting stat table for cruise:", cruise))

    # official cruise name
    official.cruise <- paste(meta[which(meta$cruise == cruise),"Cruise ID"])
    project <- paste(meta[which(meta$cruise == cruise),"Project"])
    ship <- paste(meta[which(meta$cruise == cruise),"Ship"])
    serial <- paste(meta[which(meta$cruise == cruise),"Instrument"])

    core <- paste("flow cytometry, SeaFlow, SeaFlow@UW, insitu, in-situ, biology, University of Washington, UW", official.cruise)

    var_keywords <- c("time, UTC ,date",
                    "latitude",
                    "longitude",
                    "temperature, sst",
                    "salinity, sss",
                    "light, irradiance, PAR",
                    paste("interval, confidence", core),
                    paste("Prochlorococcus, Synechococcus, Crocosphaera, picoeukaryotes, picoeuks, phytoplankton, picophytoplankton, unknown", core),
                    rep(paste("red, fluorescence, chlorophyll", core),3),
                    rep(paste("orange, fluorescence, chlorophyll", core),3),
                    rep(paste("forward scatter, FSC, FALS", core),3),
                    rep(paste("size, diameter, ESD", core),9),
                    rep(paste("quotas, carbon, biomass, POC", core),12),
                    rep(paste("abundance, concentration, count", core),2),
                    "none")


    # data
    data <- get.stat.table(db)
    data <- data[,var_data]
    
    readr::write_csv(data, path=paste0(path,"/SeaFlow_", official.cruise, "_",as.Date(Sys.time()),"_", version,".csv"))

    # dataset_metadata
    dataset_metadata <- dplyr::tibble(
                          dataset_short_name = paste0("SeaFlow_",official.cruise),
                          dataset_long_name = paste0("SeaFlow_",official.cruise),
                          dataset_version = version,
                          dataset_release_date = as.Date(Sys.time()),
                          dataset_make = "observation",
                          dataset_source = paste0("surface seawater supplied by ship (",ship,") and analyzed by SeaFlow (serial number ", serial,") using the R package Popcycle version", packageVersion("popcycle")")",
                          official_cruise_name = official.cruise,
                          dataset_ackowledgment = "This work was supported by grants from the Simons Foundation (#574495 to Francois Ribalet)"
                          dataset_email = "seaflow@uw.edu, ribalet@uw.edu",
                          dataset_description = "SeaFlow data generated by SeaFlow@UW, University of Washington (https://seaflow.netlify.app/)",
                          dataset_references = "Ribalet F, Berthiaume C, Hynes A, Swalwell J, Carlson M, Clayton S, Hennon G, Poirier C, Shimabukuro E, White A and Armbrust EV. SeaFlow data 1.0, high-resolution abundance, size and biomass of small phytoplankton in the North Pacific. ScientificData",
                          climatology = NULL)
    
    readr::write_csv(dataset_metadata, path=paste0(path,"/SeaFlow_", official.cruise,"_dataset_metadata_", version,".csv"))

    # vars_metadata
    vars_metadata <- dplyr::tibble(
                          var_short_name = var_data,
                          var_long_name,
                          var_sensor,
                          var_unit,
                          var_spatial_res = "irregular",
                          var_temporal_res = "3 minutes",
                          var_missing_value = "NA",
                          var_discipline,
                          var_keywords,
                          visualize,
                          var_comment)

    readr::write_csv(vars_metadata, path=paste0(path,"/SeaFlow_vars_metadata_",version,".csv"))

}
