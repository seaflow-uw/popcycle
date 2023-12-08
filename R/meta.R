description <- paste("The data set consists of flow cytometry-based cell abundance, cell size (equivalent spherical diameter), cellular carbon content and total carbon biomass for the cyanobacteria Prochlorococcus, Synechococcus and small-sized Crocosphaera (2-5 µm), and small eukaryotic phytoplankton (< 5 μm). The SeaFlow instrument collects the equivalent of 1 sample every 3 minutes from the ship’s flow-through seawater system. Further information can be found here https://seaflow.netlify.app/")

#' Convert data from sqlite3 database into a csv file of curated SeaFlow data for a cruise, along with metadata files.
#'
#' @param db SQLite3 database file path.
#' @param meta metadata containing cruise ID (provided by googlesheet "SeaFlow log instrument")
#' @param path Path to save the file.
#' @param version Version of the dataset.
#' @return None
#' @examples
#' \dontrun{
#'# load metadata to get offical cruise name
#'  meta <- googlesheets4::range_read('https://docs.google.com/spreadsheets/d/1Tsi7OWIZWfCQJqLDpId2aG_i-8Cp-p63PYjjvDkOtH4/edit#gid=0')
#' csv_convert(db,meta, path)
#' }
#' @export
csv_convert <- function(db, meta, path, cruisename, version = "v1.0") {


    var_short_name <- c("time", "lat", "lon",
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

    var_comment <-  c(rep('',3),
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
              "decimal deg North",
              "decimal deg East",
              "deg C",
              "psu",
              "μmol photons m-2 s-1",
              "%",
              "unitless",
              rep("unitless",9),
              rep("um",9),
              rep("pgC cell-1",12),
              rep("cells μL-1",2),
              "unitless")

    description <- description

    cruise <- sub(".outlier.db", "",basename(db))
    print(paste("formatting stat table for cruise:", basename(db)))

    # metadata
    official.cruise <- paste(meta[which(meta$cruise == cruise),"Cruise ID"])
    project <- paste(meta[which(meta$cruise == cruise),"Project"])
    ship <- paste(meta[which(meta$cruise == cruise),"Ship"])
    serial <- paste(unlist(meta[which(meta$cruise == cruise),"Instrument"]))

    # data
    data <- get_stat_table(db)
    data <- stat_calibration(data, cruise)
    data <- data[,var_short_name]
    
    #readr::write_csv(data, path=paste0(path,"/SeaFlow_", official.cruise, "_",as.Date(Sys.time()),"_", version,".csv"))
    arrow::write_parquet(data, paste0(path,"/SeaFlow_", official.cruise,"_", version,"_",as.Date(Sys.time()),".parquet"))

    # dataset_metadata
    dataset_metadata <- tibble::tibble(
                          dataset_short_name = paste0("SeaFlow_",official.cruise),
                          dataset_long_name = paste0("SeaFlow_",official.cruise),
                          dataset_version = version,
                          dataset_release_date = as.Date(Sys.time()),
                          dataset_source = paste("SeaFlow@UW, University of Washington, https://seaflow.netlify.app/"),
                          dataset_distributor = paste("http://doi.org/10.5281/zenodo.2678021"), 
                          dataset_acknolwedgment = paste("Annette Hynes, Chris Berthiaume, E Virginia Armbrust, Francois Ribalet"),
                          dataset_history = paste("Data were analyzed using the R package Popcycle version", packageVersion("popcycle")),
                          dataset_description = description,
                          dataset_references = paste("Ribalet F, Berthiaume C, Hynes A, Swalwell J, Carlson M, Clayton S, Hennon G, Poirier C, Shimabukuro E, White A and Armbrust EV. 2019 SeaFlow data 1.0, high-resolution abundance, size and biomass of small phytoplankton in the North Pacific. ScientificData 6:277 https://doi.org/10.1038/s41597-019-0292-2"))
    
    readr::write_csv(dataset_metadata, file=paste0(path,"/SeaFlow_", official.cruise,"_dataset_metadata_", version,".csv"))

    # vars_metadata
    vars_metadata <- tibble::tibble(
                          var_short_name,
                          var_long_name,
                          var_unit,
                          var_spatial_res = "irregular",
                          var_temporal_res = "three minutes",
                          var_missing_value = "NA",
                          var_comment)

    readr::write_csv(vars_metadata, file=paste0(path,"/SeaFlow_vars_metadata_",version,".csv"))

}



#' Convert Seaflow data from all cruises into Simons CMAP friendly format.
#'
#' @param path.to.dbs SQLite3 database file path.
#' @param meta SeaFlow instrument log file.
#' @param path Path to save the file.
#' @param version Version of the dataset.
#' @return None
#' @export
cmap_convert <- function(path.to.dbs, meta, path, version = "v1.3") {


    var_short_name <- c("cruise", 
            paste0("abundance_", c("prochloro","synecho","picoeuk","croco")), 
            paste0("diam_", c("prochloro","synecho","picoeuk","croco")), 
            paste0("Qc_", c("prochloro","synecho","picoeuk","croco")), 
            paste0("biomass_", c("prochloro","synecho","picoeuk","croco")))
                

    var_long_name <- c("cruise ID",
                paste0("Abundance of ", c("Prochlorococcus-like particles","Synechococcus-like particles","Picoeukaryote phytoplankton","Crocospheara-like particles")), 
                paste0("Average diameter of ", c("Prochlorococcus-like particles","Synechococcus-like particles","Picoeukaryote phytoplankton","Crocospheara-like particles")), 
                paste0("Average carbon quotas of ", c("Prochlorococcus-like particles","Synechococcus-like particles","Picoeukaryote phytoplankton","Crocospheara-like particles")), 
                paste0("Carbon biomass of ", c("Prochlorococcus population","Synechococcus population","Picoeukaryote phytoplankton population","Crocospheara population")))
                
    var_comment <-  c("cruise ID from Rolling Deck to Repository https://www.rvdata.us",
                rep("number of particles normalized by the volume of SeaFlow's virtual-core, see https://github.com/seaflow-uw/seaflow-virtualcore for more details",4),
                rep('derived from forward light scatter using the Mie theory, see https://github.com/seaflow-uw/fsc-size-calibration for more details',4),
                rep('carbon content, based on the equation pgC cell-1 = 0.261 x Volume^0.860, where Volume is calculated from cell diameter assuming spherical particle; see https://github.com/seaflow-uw/fsc-poc-calibration for more details',4),
                rep("carbon biomass = cell abundance x carbon quotas",4))

    var_unit <- c("",
              rep("cells μL-1",4),
              rep("μm",4),
              rep("pgC cell-1",4),
              rep("μgC L-1",4))

    core <- paste("insitu", "in-situ", "cruise", "reprocessed", "biology", "biogeochemistry", "optics", "flow-cytometry", "flow cytometer", "seaflow", "phytoplankton", "armbrust lab", "francois ribalet", "UW", "University of Washington", "UofW", sep=",")

    var_sensor <- "SeaFlow"

    visualize <- c(0, rep(1,16))

    var_discipline <- c("",rep("biology+biogeochemistry+optics+cytometry",16))

    description <- description
    
    today <- as.Date(Sys.time())

    data <- tibble::tibble()
    for(db in path.to.dbs){
       
        ### 1. Format DATA
        # db <- path.to.dbs[22]
        cruise <- sub(".outlier.db","",basename(db))

        # clean stat table
        clean <- get_clean_stat_table(db)

        # add columns for CMAP
        cruise.name <- paste(meta[which(meta$cruise == cruise),"Cruise ID"])
        depth <- 5
        clean.wide <- clean %>% 
                dplyr::mutate(cruise = cruise.name, depth) %>% 
                tidyr::pivot_wider(names_from = pop, values_from = c(abundance, diam, Qc, biomass), id_cols=c(time, lat, lon, depth, cruise)) %>%
                dplyr::mutate(dplyr::across(dplyr::contains(c("abundance", "biomass")), ~ tidyr::replace_na(.x, 0))) # replace NA by O for abundance and biomass

       data <- data %>% dplyr::bind_rows(clean.wide)

    }

    # arrow::write_parquet(data, paste0(path,"/SeaFlow_dataset_",version,"_", today,".parquet"))
    readr::write_csv(data, paste0(path,"/SeaFlow_dataset_",version,"_", today,".csv"))

    ## 2. dataset_metadata
    list.cruises <- paste(unique(data$cruise), collapse = ",")

    dataset_metadata <- tibble::tibble(
                          dataset_short_name = "all_SeaFlow_cruises",
                          dataset_long_name = "SeaFlow-based abundance, cell size, carbon quotas and biomass of Prochlorococcus, Synechococcus, Crocospheara and small eukaryotic phytoplankton (< 5 micron)",
                          dataset_version = version,
                          dataset_release_date = today,
                          dataset_make = "observation",
                          dataset_source = paste("SeaFlow@UW, University of Washington, https://seaflow.netlify.app/"),
                          dataset_distributor = paste("http://doi.org/10.5281/zenodo.2678021"), 
                          dataset_acknolwedgment = paste("Annette Hynes, Chris Berthiaume, E Virginia Armbrust and Francois Ribalet"),
                          dataset_history = paste("Data were analyzed using the R package Popcycle version", packageVersion("popcycle")),
                          dataset_description = description,
                          dataset_references = paste("Ribalet F, Berthiaume C, Hynes A, Swalwell J, Carlson M, Clayton S, Hennon G, Poirier C, Shimabukuro E, White A and Armbrust EV. 2019 SeaFlow data 1.0, high-resolution abundance, size and biomass of small phytoplankton in the North Pacific. ScientificData 6:277, https://doi.org/10.1038/s41597-019-0292-2"),
                          climatology="",
                          cruise_names = list.cruises)
                          
    readr::write_csv(dataset_metadata, path=paste0(path,"/SeaFlow_dataset_metadata_", version,".csv"))
    
    var_keywords <- c("",
                rep(paste("abundance,concentration,count", core, list.cruises, sep=","),4),
                rep(paste("size,diameter", core, list.cruises, sep=","),4),
                rep(paste("quotas,cellular content,carbon,POC", core, list.cruises, sep=","), 4),
                rep(paste("carbon biomass,POC", core, list.cruises, sep=","),4))


    ## 3. vars_metadata
    vars_metadata <- tibble::tibble(
                          var_short_name,
                          var_long_name,
                          var_sensor,
                          var_unit,
                          var_spatial_res = "irregular",
                          var_temporal_res = "three minutes",
                          var_discipline,
                          visualize,
                          var_keywords,
                          var_comment)

    readr::write_csv(vars_metadata, path=paste0(path,"/SeaFlow_vars_metadata_", version,".csv"))

}    