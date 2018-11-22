#' Add metadata to 'stat' table and convert it to XLSX format.
#'
#' @param db SQLite3 database file path.
#' @param meta SeaFlow instrument log file.
#' @param path Path to save the XLSX file.
#' @return None
#' @examples
#' \dontrun{
#' xls_convert(db, meta, path)
#' }
#' @export

xls_convert <- function(db, meta, path) {

    # meta <- googlesheets::gs_read(googlesheets::gs_title("SeaFlow\ instrument\ log", verbose = FALSE))

    cruise <- sub(".db", "",basename(db))
    print(paste('formatting stat table for cruise:', cruise))

    data <- get.stat.table(db)
    data <- subset(data, flag == 0)

    # add depth as metadata
    data$depth <- 5

    # Order dataframe
    data <- data[,c('time','lat','lon','depth','temp','salinity','par',
                    'quantile','pop','chl_small','pe','fsc_small',
                    'diam_lwr','diam_mid','diam_upr','Qc_lwr','Qc_mid','Qc_upr',
                    'abundance', 'abundance.se')]

    #official cruise name
    official.cruise <- paste(meta[which(meta$cruise == cruise),'Cruise ID'])

    dataset_meta_data <- dplyr::tibble(
                          dataset_short_name = official.cruise,
                          dataset_long_name = paste(official.cruise, cruise , sep=", "),
                          dataset_project = paste(meta[which(meta$cruise == cruise),'Project']),
                          dataset_version = paste('Data were analyzed using the R package Popcycle version', packageVersion('popcycle')),
                          dataset_release_date = as.Date(Sys.time()),
                          dataset_make = 'observation',
                          dataset_source = 'University of Washington / Armbrust lab (ribalet@uw.edu)',
                          dataset_history = 'NA',
                          dataset_description = 'Continuous underway measurements of picophytoplankton abundance and cell size were made using SeaFlow, see Swalwell JE, Ribalet F, Armbrust EV (2011) SeaFlow: A novel underway flow-cytometer for continuous observations of phytoplankton in the ocean. Limnology and Oceanogra- phy: Methods 9:466–477).
                                                Seawater samples were collected underway from the continuous seawater flow-through system (~5 m depth) and were prefiltered through a 100-micrometer stainless steel mesh (to eliminate large particles) prior to analysis.
                                                The flow rate of the water stream was set at 15 mL min−1 through a 200-micrometer nozzle for both cruises and for the laboratory experiments; this corresponded to an analysis rate of 15 microL min−1 by the instrument.
                                                A programmable syringe pump (Cavro XP3000, Hamilton Company) continuously injected fluorescent microspheres (1 μm, Polysciences) into the water stream as an internal standard.
                                                Data files were created every three minutes, yielding a sampling resolution along the cruise track of 1 km (for a ship cruising at ~11 knots).
                                                Data were analyzed using the R package Popcycle (https://github.com/armbrustlab/popcycle) which uses statistical clustering methods to discriminate between different phytoplankton populations, namely Prochlorococcus, Synechococcus and picoeukaryotes.
                                                For more information about the SeaFlow project, visit https://armbrustlab.ocean.washington.edu/tools/seaflow',
                          dataset_references = 'placeholder to ScientificData manuscript, in prep'
                        )



    vars_meta_data <- dplyr::tibble(
                          var_short_name = names(data),
                          var_long_name = c('time',
                                            'latitude',
                                            'longitude',
                                            'sample depth',
                                            'seawater temperature',
                                            'seawater salinity',
                                            'surface light intensity',
                                            'quantile',
                                            'population',
                                            'mean of chlorophyll fluorescence distribution',
                                            'mean of phycoerythrin fluorescence distribution',
                                            'mean of the forward light scatter distribution',
                                            'lower bound of mean cell diameter',
                                            'mid bound of mean cell diameter',
                                            'upper bound of mean cell diameter',
                                            'lower bound of mean carbon quota',
                                            'mid bound of mean carbon quota',
                                            'upper bound of mean carbon quota',
                                            'cell abundance',
                                            'standard error of cell abundance'
                                          ),
                          var_standard_name = rep(' ',ncol(data)),
                          var_sensor = c(rep('SeaFlow',1), rep("ship's broadcasted data", 2), 'none' ,rep("ship's broadcasted data", 3), rep('SeaFlow',13)),
                          var_unit = c( '%Y-%m-%dT%H:%M:%S (UTC)',
                                        'decimal degree North',
                                        'decimal degree South',
                                        'm',
                                        'deg C',
                                        'psu',
                                        'TBD',
                                        'percent',
                                        'prochloro (Prochlorococcus), synecho (Synechococcus), picoeuk (picoeukayotes),beads (internal standard), croco (Crocosphaera),unknown (unclassified particles)',
                                        'unitless',
                                        'unitless',
                                        'unitless',
                                        'micrometer',
                                        'micrometer',
                                        'micrometer',
                                        'picogram carbon per cell',
                                        'picogram carbon per cell',
                                        'picogram carbon per cell',
                                        'cells per microliter',
                                        'cells per microliter'
                                    ),
                          var_spatial_res = 'irregular',
                          var_temporal_res = '3 minutes',
                          var_missing_value = 'NA',
                          var_discipline = c(rep(' ', 4),
                                             rep('physics, abiotic', 3),
                                             rep('biology, ecology, flow cytometry',13)
                                           ),
                          var_keywords =' ',
                          var_comment = c(rep(' ',7),
                                          'quantile for OPP filtrationa (2.5 = conservative approach; 50 = standard approach; 97.5 = loosen approach, see https://github.com/armbrustlab/seaflow-filter for more details)',
                                          'population clustering based on a misture of manual gaing and semi-supervized algorithm, see https://github.com/armbrustlab/popcycle for more details',
                                          'Red fluorescence collected using a 692-40 bandpass filter',
                                          'Orange fluorescence collected using a 572-27 bandpass filter',
                                          'Forward light scatter collected using a 457-50 bandpass filter',
                                          'Mie-based estimation of cell diameter using index of refraction of 1.35, see https://github.com/armbrustlab/fsc-size-calibration for more details',
                                          'Mie-based estimation of cell diameter using index of refraction of 1.375, see https://github.com/armbrustlab/fsc-size-calibration for more details',
                                          'Mie-based estimation of cell diameter using index of refraction of 1.40, see https://github.com/armbrustlab/fsc-size-calibration for more details',
                                          'Carbon cell quotas estimates, based on lower bound of cell diameter, see https://github.com/armbrustlab/fsc-poc-calibration for more details',
                                          'Carbon cell quotas estimates, based on mid bound of cell diameter, see https://github.com/armbrustlab/fsc-poc-calibration for more details',
                                          'Carbon cell quotas estimates, based on upper bound of cell diameter, see https://github.com/armbrustlab/fsc-poc-calibration for more details',
                                          'mean of cell abundance, see https://github.com/armbrustlab/seaflow-virtualcore for more details',
                                          'standard error based on uncertainties in converting sample stream pressure to flow rate, see https://github.com/armbrustlab/seaflow-virtualcore for more details'
                                      )
                        )


      openxlsx::write.xlsx(x=list(data, dataset_meta_data, vars_meta_data), file=path, sheetName=c('data','dataset_meta_data','vars_meta_data'))

}
