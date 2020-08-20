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

var_standard_name <- c("time",
                  "latitude",
                  "longitude",
                  "sea surface temperature",
                  "sea surface salinity",
                  "sea surface solar irradiance",
                  "OPP confidence interval",
                  "population",
                  "first quartile of chlorophyll fluorescence",
                  "median of chlorophyll fluorescence",
                  "third quartile of chlorophyll fluorescence",
                  "first quartile of phycoerythrin fluorescence",
                  "median of phycoerythrin fluorescence",
                  "third quartile of phycoerythrin fluorescence",
                  "first quartile of forward scatter",
                  "median of the forward scatter",
                  "third quartile of forward scatter",
                  "first quartile of equivalent spherical diameter using high refractive index",
                  "median of equivalent spherical diameter using high refractive index",
                  "third quartile of equivalent spherical diameter using high refractive index",
                  "first quartile of equivalent spherical diameter using mid refractive index",
                  "median of equivalent spherical diameter using mid refractive index",
                  "third quartile of equivalent spherical diameter using mid refractive index",
                  "first quartile of equivalent spherical diameter using low refractive index",
                  "median of equivalent spherical diameter using low refractive index",
                  "third quartile of equivalent spherical diameter using low refractive index",
                  "first quartile of cellular carbon content using high refractive index",
                  "median of cellular carbon content using high refractive index",
                  "mean of cellular carbon content using high refractive index",
                  "third quartile of cellular carbon content using high refractive index",
                  "first quartile of cellular carbon content using mid refractive index",
                  "median of cellular carbon content using mid refractive index",
                  "mean of cellular carbon content using mid refractive index",
                  "third quartile of cellular carbon content using mid refractive index",
                  "first quartile of cellular carbon content using low refractive index",
                  "median of cellular carbon content using low refractive index",
                  "mean of cellular carbon content using low refractive index",
                  "third quartile of cellular carbon content using low refractive index",
                  "cell concentration",
                  "standard error of cell aconcentration",
                  "status flag")

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
                  rep('cell diameter based on Mie theory using an index of refraction of 1.35 for phytoplanktonand 1.337 for seawater, see https://github.com/seaflow-uw/fsc-size-calibration for more details',3),
                  rep('carbon content based on the equation pgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter assuming spherical particle (Mie-based, using refractive index of 1.055); see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                  rep('carbon content based on the equation pgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter assuming spherical particle (Mie-based, using refractive index of 1.032); see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                  rep('carbon content based on the equation pgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter assuming spherical particle (Mie-based, using refractive index of 1.017); see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
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

var_keywords <- c("time, UTC, date",
                "latitude",
                "longitude",
                "temperature, sst",
                "salinity, sss",
                "light, irradiance, PAR",
                "interval, confidence",
                "Prochlorococcus, Synechococcus, Crocosphaera, picoeukaryotes, phytoplankton, picophytoplankton, unknown",
                rep("red, fluorescence, chlorophyll",3),
                rep("orange, fluorescence, chlorophyll",3),
                rep("forward, angle, light, scatter, FSC, FALS",3),
                rep("size, diameter",9),
                rep("quotas, carbon, biomass, POC",12),
                rep("abundance, concentration, density",2),
                "none")

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





#' Convert data from sqlite3 database into a csv file of curated SeaFlow data for a cruise, along with metadata files.
#'
#' @param db SQLite3 database file path.
#' @param path Path to save the file.
#' @param version Version of the dataset.
#' @return None
#' @examples
#' \dontrun{
#' csv_convert(db, meta, path)
#' }
#' @export
csv_convert <- function(db, path, version = "v1.0") {

    meta <- googlesheets4::sheets_read(as_sheets_id('https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4/edit#gid=0', verbose = FALSE))

    cruise <- sub(".db", "",basename(db))
    print(paste("formatting stat table for cruise:", cruise))

    # official cruise name
    official.cruise <- paste(meta[which(meta$cruise == cruise),"Cruise ID"])
    project <- paste(meta[which(meta$cruise == cruise),"Project"])
    ship <- paste(meta[which(meta$cruise == cruise),"Ship"])
    serial <- paste(unlist(meta[which(meta$cruise == cruise),"Instrument"]))

    # data
    data <- get.stat.table(db)
    data <- stat.calibration(data, cruise)
    data <- data[,var_data]
    readr::write_csv(data, path=paste0(path,"/SeaFlow_", official.cruise, "_dataset_", version,".csv"))

    # dataset_metadata
    dataset_metadata <- dplyr::tibble(
                          dataset_short_name = paste0("SeaFlow_",official.cruise),
                          dataset_long_name = paste0("SeaFlow_",official.cruise),
                          dataset_cruise = official.cruise,
                          dataset_project = project,
                          dataset_version = version,
                          dataset_release_date = as.Date(Sys.time()),
                          dataset_make = "observation",
                          dataset_source = paste0("surface seawater supplied by ship ",ship," and analyzed by SeaFlow (serial number ", serial,")"),
                          dataset_doi = "https://doi.org/10.5281/zenodo.XXXXX",
                          dataset_history = paste("Data were analyzed using the R package Popcycle version", packageVersion("popcycle")),
                          dataset_description = "SeaFlow data generated by University of Washington / Armbrust lab (ribalet@uw.edu). Visit https://seaflow-uw.ocean.washington.edu/tools/seaflow for more information about the SeaFlow project",
                          dataset_references = "Ribalet F, Berthiaume C, Hynes A, Swalwell J, Carlson M, Clayton S, Hennon G, Poirier C, Shimabukuro E, White A and Armbrust EV. SeaFlow data 1.0, high-resolution abundance, size and biomass of small phytoplankton in the North Pacific. ScientificData")
    readr::write_csv(dataset_metadata, path=paste0(path,"/SeaFlow_", official.cruise,"_dataset_metadata_", version,".csv"))

    # vars_metadata
    vars_metadata <- dplyr::tibble(
                          var_short_name = var_data,
                          var_standard_name,
                          var_sensor,
                          var_unit,
                          var_spatial_res = "irregular",
                          var_temporal_res = "3 minutes",
                          var_missing_value = "NA",
                          var_discipline,
                          var_keywords,
                          var_comment)
    readr::write_csv(vars_metadata, path=paste0(path,"/SeaFlow_vars_metadata.csv"))

}



var_data2 <- c("time", "lat", "lon", "cruise", 
            paste0("abundance_", c("prochloro","synecho","picoeuk","croco")), 
            paste0("diam_", c("prochloro","synecho","picoeuk","croco")), 
            paste0("Qc_", c("prochloro","synecho","picoeuk","croco")), 
            paste0("biomass_", c("prochloro","synecho","picoeuk","croco")))
                

var_standard_name2 <- c("time", "latitude", "longitude", "cruise ID",
                paste0("abundance of ", c("Prochlorococcus-like particles","Synechococcus-like particles","Picoeukaryote phytoplankton","Crocospheara-like particles")), 
                paste0("Diameter of ", c("Prochlorococcus-like particles","Synechococcus-like particles","Picoeukaryote phytoplankton","Crocospheara-like particles")), 
                paste0("Carbon quotas of ", c("Prochlorococcus-like particles","Synechococcus-like particles","Picoeukaryote phytoplankton","Crocospheara-like particles")), 
                paste0("Carbon biomass of ", c("Prochlorococcus population","Synechococcus population","Picoeukaryote phytoplankton population","Crocospheara population")))
                
var_comment2 <-  c(rep("",3),
                "cruise ID from Rolling Deck to Repository https://www.rvdata.us",
                rep("cell abundance, cal;culated based on teh number of particles normalized by the volume of SeaFlow's virtual-core, see https://github.com/seaflow-uw/seaflow-virtualcore for more details",4),
                rep('cell diameter based on Mie theory, see https://github.com/seaflow-uw/fsc-size-calibration for more details',4),
                rep('carbon content based on the equation pgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter assuming spherical particle; see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                rep("carbon biomass = cell abundance x carbon quotas",4))

var_unit2 <- c("%Y-%m-%dT%H:%M:%S",
              "decimal degree North",
              "decimal degree East",
              "",
              rep("cells per microliter",4),
              rep("micron",4),
              rep("pgC per cell",4),
              rep("pgC per liter",4))

var_keywords2 <- c(rep("",4),
                rep("abundance, concentration, count",4),
                rep("size, diameter",4),
                rep("quotas , cellular content, carbon, POC",4),
                rep("carbon biomass, POC",4))

var_sensor2 <- "SeaFlow"

var_discipline2 <- c(rep("", 4),
                   rep("biology, biogeochemistry, optics, cytometry, SeaFlow",16))

#' Convert Seaflow data from all cruises into Simons CMAP friendly format.
#'
#' @param path.to.dbs SQLite3 database file path.
#' @param meta SeaFlow instrument log file.
#' @param path Path to save the file.
#' @param version Version of the dataset.
#' @return None
#' @examples
#' \dontrun{
#' csv_convert(db, meta, path)
#' }
#' @export
cmap_convert <- function(path.to.dbs, path, version = "v1.3") {

    meta <- googlesheets4::sheets_read(as_sheets_id('https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4/edit#gid=0', verbose = FALSE))
    today <- as.Date(Sys.time())

    data <- tibble()
    for(db in path.to.dbs){
       
        ### 1. Format DATA
        # db <- path.to.dbs[24]
        cruise <- sub(".db","",basename(db))

        # clean stat table
        clean <- get.clean.stat.table(db, pop="prochloro", ref_diam=0.54)

        # add columns for CMAP
        cruise.name <- paste(meta[which(meta$cruise == cruise),"Cruise ID"])
        depth <- 5
        clean.wide <- clean %>% 
                mutate(cruise = cruise.name, depth) %>% 
                pivot_wider(names_from = pop, values_from = c(abundance, diam, Qc, biomass), id_cols=c(time, lat, lon, depth, cruise))  

       data <- data %>% bind_rows(clean.wide)

    }

    # arrow::write_parquet(data, paste0(path,"/SeaFlow_dataset_",version,"_", today,".parquet"))
    readr::write_csv(data, paste0(path,"/SeaFlow_dataset_",version,"_", today,".csv"))

    ## 2. dataset_metadata
    dataset_metadata <- dplyr::tibble(
                          dataset_short_name = "all SeaFlow cruises",
                          dataset_long_name = "Abundance, cell size, carbon quotas and biomass of Prochlorococcus, Synechooccus, Crocospheara and small picoeuks (< 5 micron)",
                          dataset_version = version,
                          dataset_release_date = today,
                          dataset_make = "observation",
                          dataset_source = "SeaFlow serial_id 740, 751 and 989",
                          dataset_doi = "https://doi.org/10.5281/zenodo.XXXXX",
                          dataset_history = paste("Data were analyzed using the R package Popcycle version", packageVersion("popcycle")),
                          dataset_description = "SeaFlow data generated by University of Washington / Armbrust lab (ribalet@uw.edu). Visit https://seaflow-uw.ocean.washington.edu/tools/seaflow for more information about the SeaFlow project",
                          dataset_references = "Ribalet F, Berthiaume C, Hynes A, Swalwell J, Carlson M, Clayton S, Hennon G, Poirier C, Shimabukuro E, White A and Armbrust EV. SeaFlow data 1.0, high-resolution abundance, size and biomass of small phytoplankton in the North Pacific. ScientificData")
    
    readr::write_csv(dataset_metadata, path=paste0(path,"/SeaFlow_dataset_metadata_", version,".csv"))

    ## 3. vars_metadata
    vars_metadata <- dplyr::tibble(
                          var_short_name = var_data2,
                          var_standard_name2,
                          var_sensor2,
                          var_unit2,
                          var_spatial_res = "irregular",
                          var_temporal_res = "3 minutes",
                          var_missing_value = "NA",
                          var_discipline2,
                          var_keywords2,
                          var_comment2)

    readr::write_csv(vars_metadata, path=paste0(path,"/SeaFlow_vars_metadata_", version,".csv"))

}    