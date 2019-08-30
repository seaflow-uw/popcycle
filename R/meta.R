var_standard_name <- c("time",
                  "latitude",
                  "longitude",
                  "sea surface temperature",
                  "sea surface salinity",
                  "sea surface solar irradiance",
                  "OPP confidence interval",
                  "none",
                  "first quartile of chlorophyll fluorescence",
                  "median of chlorophyll fluorescence",
                  "third quartile of chlorophyll fluorescence",
                  "first quartile of phycoerythrin fluorescence",
                  "median of phycoerythrin fluorescence",
                  "third quartile of phycoerythrin fluorescence",
                  "first quartile of forward scatter",
                  "median of the forward scatter",
                  "third quartile of forward scatter",
                  "first quartile of equivalent spherical diameter using low refractive index",
                  "median of equivalent spherical diameter using low refractive index",
                  "third quartile of equivalent spherical diameter using low refractive index",
                  "first quartile of equivalent spherical diameter using mid refractive index",
                  "median of equivalent spherical diameter using mid refractive index",
                  "third quartile of equivalent spherical diameter using mid refractive index",
                  "first quartile of equivalent spherical diameter using high refractive index",
                  "median of equivalent spherical diameter using high refractive index",
                  "third quartile of equivalent spherical diameter using high refractive index",
                  "first quartile of cellular carbon content using low refractive index",
                  "median of cellular carbon content using low refractive index",
                  "mean of cellular carbon content using low refractive index",
                  "third quartile of cellular carbon content using low refractive index",
                  "first quartile of cellular carbon content using mid refractive index",
                  "median of cellular carbon content using mid refractive index",
                  "mean of cellular carbon content using mid refractive index",
                  "third quartile of cellular carbon content using mid refractive index",
                  "first quartile of cellular carbon content using high refractive index",
                  "median of cellular carbon content using high refractive index",
                  "mean of cellular carbon content using high refractive index",
                  "third quartile of cellular carbon content using high refractive index",
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
                  "population",
                  "25% percentile of chlorophyll fluorescence",
                  "50% percentile of chlorophyll fluorescence",
                  "75% percentile of chlorophyll fluorescence",
                  "25% percentile of phycoerythrin fluorescence",
                  "50% percentile of phycoerythrin fluorescence",
                  "75% percentile of phycoerythrin fluorescence",
                  "25% percentile of forward scatter",
                  "50% percentile of the forward scatter",
                  "75% percentile of forward scatter",
                  "25% percentile of equivalent spherical diameter using low refractive index",
                  "50% percentile of equivalent spherical diameter using low refractive index",
                  "75% percentile of equivalent spherical diameter using low refractive index",
                  "25% percentile of equivalent spherical diameter using mid refractive index",
                  "50% percentile of equivalent spherical diameter using mid refractive index",
                  "75% percentile of equivalent spherical diameter using mid refractive index",
                  "25% percentile of equivalent spherical diameter using high refractive index",
                  "50% percentile of equivalent spherical diameter using high refractive index",
                  "75% percentile of equivalent spherical diameter using high refractive index",
                  "25% percentile of cellular carbon content using low refractive index",
                  "50% percentile of cellular carbon content using low refractive index",
                  "linear mean of cellular carbon content using low refractive index",
                  "75% percentile of cellular carbon content using low refractive index",
                  "25% percentile of cellular carbon content using mid refractive index",
                  "50% percentile of cellular carbon content using mid refractive index",
                  "linear mean of cellular carbon content using mid refractive index",
                  "75% percentile of cellular carbon content using mid refractive index",
                  "25% percentile of cellular carbon content using high refractive index",
                  "50% percentile of cellular carbon content using high refractive index",
                  "linear mean of cellular carbon content using high refractive index",
                  "75% percentile of cellular carbon content using high refractive index",
                  "cell abundance",
                  "standard error of cell abundance",
                  "outliers")

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
              rep("picogram carbon per cell",9),
              rep("cells per microliter",2),
              "unitless")

var_keywords <- c("time+UTC+date",
                "latitude",
                "longitude",
                "temperature+sst",
                "salinity+sss",
                "light+irradiance+PAR",
                "interval+confidence",
                "Prochlorococcus+Synechococcus+Crocosphaera+picoeukaryotes+phytoplankton+picophytoplankton",
                rep("red+fluorescence+chlorophyll",3),
                rep("orange+fluorescence+chlorophyll",3),
                rep("forward+angle+light+scatter+FSC+FALS",3),
                rep("size+diameter",9),
                rep("quotas+carbon+biomass+POC",9),
                rep("abundance+concentration+density",2),
                "none")

var_sensor <- c("none",
                rep(paste("data broadcasted by", ship), 2),
                rep(paste("data broadcasted by", ship), 3),
                rep(paste0("SeaFlow (serial number (", serial,")"),14))

var_discipline <- c(rep("none", 3),
                   rep("physics+abiotic", 3),
                   "optics+SeaFlow",
                   rep("optics+cytometry+SeaFlow",9),
                   rep("biology+biogeochemistry+optics+cytometry+SeaFlow",18),
                   rep("biology+biogeochemistry",2),
                   "none")

#' @param db SQLite3 database file path.
#' @param meta SeaFlow instrument log file.
#' @param path Path to save the XLSX file.
#' @return None
#' @examples
#' \dontrun{
#' csv_convert(db, meta, path)
#' }
#' @export

csv_meta <- function(db, meta, path, version = "v1.2") {

    # meta <- googlesheets::gs_read(googlesheets::gs_title("SeaFlow\ instrument\ log", verbose = FALSE))

    cruise <- sub(".db", "",basename(db))
    print(paste("formatting stat table for cruise:", cruise))

    data <- get.stat.table(db)

    #official cruise name
    official.cruise <- paste(meta[which(meta$cruise == cruise),"Cruise ID"])
    project <- paste(meta[which(meta$cruise == cruise),"Project"])
    ship <- paste(meta[which(meta$cruise == cruise),"Ship"])
    serial <- paste(meta[which(meta$cruise == cruise),"Instrument"])

    dataset_meta_data <- dplyr::tibble(
                          dataset_short_name = paste0("SeaFlow_",official.cruise),
                          dataset_long_name = paste0("SeaFlow_",official.cruise),
                          dataset_cruise = official.cruise,
                          dataset_project = project,
                          dataset_version = version,
                          dataset_release_date = as.Date(Sys.time()),
                          dataset_make = "observation",
                          dataset_source = paste0("SeaFlow (serial number ", serial, ") deployed on ", ship),
                          dataset_doi = "https://doi.org/10.5281/zenodo.XXXXX",
                          dataset_history = paste("Data were analyzed using the R package Popcycle version", packageVersion("popcycle")),
                          dataset_description = "SeaFlow data generated by University of Washington / Armbrust lab (ribalet@uw.edu). Visit https://armbrustlab.ocean.washington.edu/tools/seaflow for more information about the SeaFlow project",
                          dataset_references = "Ribalet F, Berthiaume C, Hynes A, Swalwell J, Carlson M, Clayton S, Hennon G, Poirier C, Shimabukuro E, White A and Armbrust EV. SeaFlow data 1.0, high-resolution abundance, size and biomass of small phytoplankton in the North Pacific. ScientificData"
                          )

    vars_meta_data <- dplyr::tibble(
                          var_short_name = names(data),
                          var_standard_name,
                          var_sensor,
                          var_unit,
                          var_spatial_res = "irregular",
                          var_temporal_res = "3 minutes",
                          var_missing_value = "NA",
                          var_discipline,
                          var_keywords,
                          var_comment)

  write_csv(dataset_meta_data, file=paste0(path, "_dataset_meta_data.csv")
  write_csv(vars_meta_data, file=paste0(path, "_vars_meta_data.csv")
}
