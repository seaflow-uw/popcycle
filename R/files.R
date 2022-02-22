#' Find EVT files with a recursive search down a directory tree.
#'
#' @param evt_dir Directory containing EVT files.
#' @param db SQLite3 popcycle database file. If present, inner join to sfl table
#'   and add a date column.
#' @return Tibble of EVT file paths, file IDs, and optionally dates: ("path",
#'   "file_id", "date").
#' @export
get_evt_files <- function(evt_dir, db = NULL) {
  file_list <- sort(list.files(evt_dir, recursive = T, full.names = TRUE))

  # regexp to match both types of EVT files
  #   - 37.evt (old style)
  #   - 2014-05-15T17-07-08+0000 or 2014-07-04T00-03-02+00-00 (new style)
  # In the new style the final timezone offset may not always be UTC (00-00)
  # so be sure to correctly parse it in all code.
  regexp <- "/?[0-9]+\\.evt(\\.gz)?$|/?[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}-[0-9]{2}-[0-9]{2}[+-][0-9]{2}-?[0-9]{2}(\\.gz)?$"
  file_list <- file_list[grep(regexp, file_list)]

  clean_file_list <- clean_file_path(file_list)
  files <- tibble::tibble(path = file_list, file_id = as.character(clean_file_list))

  if (!is.null(db)) {
    sfl <- get_sfl_table(db) %>%
      dplyr::select(date, file_id = file)
    files <- dplyr::inner_join(files, sfl, by = "file_id") %>%
      dplyr::relocate(date)
  }

  return(files)
}

#' Clean a file path.
#'
#' Convert an EVT/OPP/VCT file path to a form suitable for storage in the SQLite
#' db. Any ".gz", ".opp", ".vct" extensions will be removed.
#'
#' @param paths Character vector of file paths to clean.
#' @return Modified file path as julian_day/EVT_file_name.
#' @export
clean_file_path <- function(paths) {
  cleaner <- function(fpath) {
    # Clean up any places with multiple "/"s
    fpath <- gsub("/+", "/", fpath)

    # Check for julian day directory
    parts <- unlist(strsplit(fpath, "/"))
    if (length(parts) < 2) {
      stop(paste0("file path ", fpath, " must contain a julian day directory"))
    }

    file.name <- parts[length(parts)]
    julian.day <- parts[length(parts)-1]

    julian.regexp <- "^[0-9]{4}_[0-9]+$"
    if (length(grep(julian.regexp, julian.day)) != 1) {
      stop(paste0("Julian day directory does not match pattern YYYY_day in ", fpath))
    }

    # Get rid of any .gz extension
    if (nchar(file.name) >= 3) {
      if (substr(file.name, nchar(file.name) - 2, nchar(file.name)) == ".gz") {
        file.name <- substr(file.name, 1, nchar(file.name) - 3)
      }
    }

    # Get rid of any .opp extension
    if (nchar(file.name) >= 4) {
      if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) == ".opp") {
        file.name <- substr(file.name, 1, nchar(file.name) - 4)
      }
    }

    # Get rid of any .vct extension
    if (nchar(file.name) >= 4) {
      if (substr(file.name, nchar(file.name) - 3, nchar(file.name)) == ".vct") {
        file.name <- substr(file.name, 1, nchar(file.name) - 4)
      }
    }
    return(paste(julian.day, file.name, sep="/"))
  }
  cleaned <- sapply(paths, cleaner)
  names(cleaned) <- NULL
  return(cleaned)
}

#' Get EVT data frame by file.
#'
#' @param evt.dir EVT file directory.
#' @param file_id File ID with julian day directory. Can be a single
#'   file name or vector of file names.
#' @param transform Linearize EVT data. Default is FALSE.
#' @return Data frame of EVT data for all files in file.name.
#' @export
get_evt_by_file <- function(evt_dir, file_id, transform = FALSE) {
  evt_files <- clean_file_path(file_id)
  evt_reader <- function(f) {
    return(readSeaflow(file.path(evt_dir, f), transform = transform))
  }
  evts <- lapply(evt_files, evt_reader)
  if (length(evts) > 1) {
    return(dplyr::bind_rows(evts))
  }
  return(evts[[1]])
}

#' Get tibble of filtered particles selecting by file IDs.
#'
#' @param db SQLite3 database file path.
#' @param opp_dir OPP file directory.
#' @param file_ids Vector of file IDs with julian day directory.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of filtered particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_opp_by_file <- function(db, opp_dir, file_ids, outliers = TRUE, col_select = NULL) {
  return(get_processed_particles(db, opp_dir, "opp", file_ids = file_ids,
                                 outliers = outliers, col_select = col_select))
}

#' Get tibble of filtered particles selecting by date.
#'
#' Datetime string formats should be in the form "2020-07-31 10:00".
#'
#' @param db SQLite3 database file path.
#' @param opp_dir OPP file directory.
#' @param start_date Start date, inclusive.
#' @param end_date End date, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of filtered particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_opp_by_date <- function(db, opp_dir, start_date, end_date, outliers = TRUE,
                            col_select = NULL) {
  return(get_processed_particles(db, opp_dir, "opp", start_date = start_date,
                                 end_date = end_date, outliers = outliers,
                                 col_select = col_select))
}

#' Get tibble of classified particles selecting by file IDs.
#'
#' @param db SQLite3 database file path.
#' @param vct_dir VCT file directory.
#' @param file_ids Vector of file IDs with julian day directory.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of classified particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_vct_by_file <- function(db, vct_dir, file_ids, outliers = TRUE, col_select = NULL) {
  return(get_processed_particles(db, vct_dir, "vct", file_ids = file_ids,
                                 outliers = outliers, col_select = col_select))
}

#' Get tibble of classified particles selecting by date.
#'
#' Datetime string formats should be in the form "2020-07-31 10:00".
#'
#' @param db SQLite3 database file path.
#' @param vct_dir VCT file directory.
#' @param start_date Start date, inclusive.
#' @param end_date End date, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a character vector. date and file_id are always added as the first columns.
#'   By default all columns are returned.
#' @return Tibble of classified particles. If no data is found an empty tibble
#'   will be returned.
#' @export
get_vct_by_date <- function(db, vct_dir, start_date, end_date, outliers = TRUE,
                            col_select = NULL) {
  return(get_processed_particles(db, vct_dir, "vct", start_date = start_date,
                                 end_date = end_date, outliers = outliers,
                                 col_select = col_select))
}

#' Get a tibble of OPP or VCT particles
#'
#' @param db SQLite3 database file path.
#' @param data_dir Directory with OPP or VCT parquet files.
#' @param data_type "opp" or "vct".
#' @param file_ids Vector of file IDs with julian day directory.
#' @param start_date Start date, inclusive.
#' @param end_date End date, inclusive.
#' @param outliers If TRUE, remove outliers. Otherwise don't perform outlier
#'   filtering.
#' @param col_select col_select parameter passed to arrow::read_parquet as
#'   a charactr vector. date and file_id are always added to to this vector. By
#'   default all columns are returned.
#' @return Tibble of OPP or VCT particles. If no data is found a tibble with
#'   zero rows will be returned.
get_processed_particles <- function(db, data_dir, data_type, file_ids = NULL,
                                    start_date = NULL, end_date = NULL,
                                    outliers = TRUE, col_select = NULL) {
  if (data_type == "opp") {
    table_getter <- get_opp_table
    path_adder <- add_opp_paths
    window_path_col <- "window_opp_path"
  } else if (data_type == "vct") {
    table_getter <- get_vct_table
    path_adder <- add_vct_paths
    window_path_col <- "window_vct_path"
  } else {
    stop("data_type must be 'opp' or 'vct'")
  }

  # Clean up parameters
  if (!is.null(file_ids)) {
    file_ids <- clean_file_path(file_ids)
  }
  # Alway make sure date and file_id are retrieved
  if (!is.null(col_select)) {
    col_select <- c("date", "file_id", col_select[!(col_select %in% c("date", "file_id"))])
  }
  # Make sure predicted and discovered hourly Parquet paths are comparable
  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  # Build file ID list
  to_get <- table_getter(db) %>%
    dplyr::filter(quantile == 50) %>% # only need one quantile to list files
    dplyr::select(date, file_id, flag)
  if (outliers) {
    to_get <- to_get %>% dplyr::filter(flag == FLAG_OK)
  }
  if (!is.null(file_ids)) {
    to_get <- to_get %>% dplyr::filter(file_id %in% file_ids)
  }
  if (!is.null(start_date)) {
    to_get <- to_get %>% dplyr::filter(date >= start_date)
  }
  if (!is.null(end_date)) {
    to_get <- to_get %>% dplyr::filter(date <= end_date)
  }

  if (nrow(to_get) == 0) {
    # No data found
    warning("no data found")
    return(tibble::tibble())
  }

  # Get hourly parquet files on disk
  hourly_files <- list.files(data_dir, pattern = paste0("*.", data_type, ".parquet"), full.names = TRUE)

  # Predict hourly parquet paths for each file, filter down to those on disk
  to_get <- path_adder(to_get, data_dir) %>%
    dplyr::filter(.data[[window_path_col]] %in% hourly_files) %>%
    dplyr::arrange(date)

  if (nrow(to_get) == 0) {
    # No data found
    warning("no corresponding parquet files found")
    return(tibble::tibble())
  }

  df <- to_get %>%
    dplyr::group_by(across(all_of(window_path_col))) %>%
    dplyr::group_map(function(rows, key) {
      arrow::read_parquet(key[[window_path_col]], col_select = {{ col_select }}) %>%
        dplyr::filter(file_id %in% rows$file_id)
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_if(is.factor, forcats::fct_drop)

  # Check for file IDs not found in parquet files
  not_found <- !(to_get$file_id %in% df$file_id)
  if (sum(not_found) > 0) {
    warning("some file IDs not found:\n", paste(to_get$file_id[not_found], collapse = "\n"))
  }

  return(df)
}

#' Create an empty EVT data frame
#'
#' @return EVT data frame with no rows
empty_evt <- function() {
  df <- data.frame(matrix(ncol = length(EVT.HEADER), nrow = 0))
  colnames(df) <- EVT.HEADER
  return(df)
}

#' Create an empty OPP data frame
#'
#' @return OPP data frame with no rows
empty_opp <- function() {
  return(empty_evt())
}

#' Read Mie theory calibration file
#'
#' @return A dataframe of Mie theory conversion values
#' @export
read_mie_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("scatter", paste0("calibrated-mie.csv"),package="popcycle")
  }
  return(read.csv(path))
}

#' Read Abundance calibration file
#'
#' @return A dataframe of regression values (cruise=cruise.name, pop=prochloro, a=slope, b=intercept)
#' @export
read_calib_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("abundance", paste0("abundance-calibration.csv"),package="popcycle")
  }
  return(read.csv(path))
}

#' Read table of indexes of refraction for cruise and population combinations
#'
#' @return A dataframe of with columns for cruise and each population's index of refraction alias
#' @export
read_refraction_csv <- function(path=NULL) {
  if (is.null(path)) {
    path <- system.file("scatter", paste0("RefracIndices_percruise.csv"), package="popcycle")
  }
  return(read.csv(path))
}


#' Read SFL tab-delimited file
#' @param path Path to validated tab-delimited SFL file
#' @return A tibble with SFL data
#' @export
read_sfl_tsv <- function(path) {
  sfl_data <- readr::read_delim(
    path,
    delim = "\t",
    col_types = list(
      FILE = readr::col_character(),
      DATE = readr::col_character(),
      `FILE DURATION` = readr::col_double(),
      LAT = readr::col_character(),
      LON = readr::col_character(),
      CONDUCTIVITY = readr::col_double(),
      SALINITY = readr::col_double(),
      `OCEAN TEMP` = readr::col_double(),
      PAR = readr::col_double(),
      `BULK RED` = readr::col_double(),
      `STREAM PRESSURE` = readr::col_double(),
      `EVENT RATE` = readr::col_double()
    )
  ) %>% rename(
    file = FILE,
    date = DATE,
    file_duration = `FILE DURATION`,
    lat = LAT,
    lon = LON,
    conductivity = CONDUCTIVITY,
    salinity = SALINITY,
    ocean_tmp = `OCEAN TEMP`,  # not a typo
    par = PAR,
    bulk_red = `BULK RED`,
    stream_pressure = `STREAM PRESSURE`,
    event_rate = `EVENT RATE`
  )
  return(sfl_data)
}


#' Manipulate the size distribution created by create_PSD(). 
#' Calculate the sum of particles in each size class over specific temporal resolution; transform the header
#'
#' @param PSD Particle size disitribution created by create_PSD().
#'  i.e., a tibble of size distribution over time. First column must be time (POSIXt class object);
#'  Second column must name of the population; other columns represent the different size classes. 
#'  Size classes can represent either diameter or carbon quota (assuming spherical particles).
#' @param time.step Time step over which to sum the number of particles in each size class. Default 1 hour, must be higher than 3 minutes
#' @param Qc.to.diam Convert carbon quotas to diameter as described in
#'  Menden-Deuer, S. and Lessard, E. J. Carbon to volume relationships for dinoflagellates, diatoms, and other protist plankton.
#'  Limnol. Oceanogr. 45, 569–579 (2000).
#' @param abundance.to.biomass Calculate carbon biomass in each population (i.e. cell abundance x Qc)
#' @param interval.to.geomean Transform size class intervals to geometric mean values 
#' (i.e. convert breaks (min, max] to geometric mean defined as sqrt(mean*max). 
#' @return Size distribution 
#' @name transform_PSD
#' @examples
#' \dontrun{
#' PSD <- transform_PSD(PSD, time.step="1 hour")
#' }
#' @export
transform_PSD <- function(PSD, time.step="1 hour", 
                                        Qc.to.diam=FALSE, 
                                        interval.to.geomean=FALSE,
                                        abundance.to.biomass=FALSE){
  
  # Check that 'time' is a POSIXt class object 
  if(! lubridate::is.POSIXt(PSD$date)){
  print("Date is not recognized as POSIXt class")
  stop
  }

  # Check that 'pop' column is there 
  if(!any(names(PSD)=='pop')){
    print("column 'pop' is missing")
  stop
  }

   # Calculate the mean in each size class over new time interval
  if(!is.null(time.step)){
    PSD  <- PSD  %>%
              dplyr::group_by(date = cut(date, breaks=time.step), pop) %>%
              dplyr::summarise_all(mean)
  }                    



  # Menden-Deuer, S. & Lessard conversion factors
  d <- 0.261; e <- 0.860
  # select column that have PSD data
  clmn <- grep("]", names(PSD))
  # convert size interval (factors) into data.frame
  breaks <- strsplit(sub("\\]","",sub("\\(","",colnames(PSD)[clmn])),",")


  if(Qc.to.diam){
    #convert Qc into diam using the Menden-Deuer conversion
    b <- lapply(breaks, function(x) round(2*(3/(4*pi)*(as.numeric(x)/d)^(1/e))^(1/3),6))
    colnames(PSD)[clmn] <- sub("\\)","\\]", sub("c","",as.character(b)))
  }

  if(interval.to.geomean){
    # transform size class intervals to mean values (i.e. convert breaks (min, max] to geom mean). 
    if(Qc.to.diam){
      midval <- unlist(list(lapply(b, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
    }else{
      midval <- unlist(list(lapply(breaks, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
      }
    colnames(PSD)[clmn] <- round(midval,4)
  }
  
  if(abundance.to.biomass){
    # calculate biomass in each bin (ugC L-1) = Qc(pgC cell-1) x Abundance (cells uL-1)
    midval <- unlist(list(lapply(breaks, function(x) sqrt(mean(as.numeric(x))*max(as.numeric(x))))))
    PSD[,clmn] <-  t(diag(midval) %*%  t(as.matrix(PSD[,clmn])))
  }

  # time converted to factor needs to be converted back to POSIXt
  PSD$date <- as.POSIXct(PSD$date, tz='GMT')

  return(PSD)

}

#' Clean the stat table by selecting the closest refractive index to the reference diameter for a particular population,
#' keep only the clean data (flag ==0 ) and remove percentile column
#'
#' @param db SQLite3 database file path.
#' @param pop Name of the population. Can be either "prochloro", "synecho", "picoeuk", "croco"
#' @param ref_diam Diameter value (in micron)
#' @return A cleaned stat table with corrected refractive index
#' @export
get_clean_stat_table <- function(db, pop="prochloro", ref_diam=0.54){

  cruise <- sub(".db","",basename(db))
  print(cruise)
  stat <- tibble::as_tibble(get_stat_table(db))
  stat <- stat_calibration(stat, cruise)

  ### Select the appropriate refractive index for each population
  ri <- tibble::tibble(lwr=1.055, mid=1.032, upr=1.017)

  # Select the appropriate refractive index
  ref <- as.numeric(ref_diam)
  phyto <- as.character(pop)

  p  <- stat %>%
        dplyr::filter(flag == 0, quantile == 2.5, pop == phyto) %>%
        dplyr::select(diam_lwr_med, diam_mid_med, diam_upr_med) %>%
        dplyr::summarise_all(mean)
  best <- which(abs(p - ref) == min(abs(p - ref)))
  refrac <- c("lwr","mid","upr")[best[1]]
        
  if(is.na(refrac)) refrac <- "mid" # case when there is no population of interest

  print(paste0("best refractive index for ",phyto," : ",ri[refrac] ," (",refrac, ")"))
            
  # choice of refractive index for Prochloro, Synecho, picoeuk and Croco, respectively
  n <- c(refrac, refrac, "lwr","lwr") 

  # select the appropriate data
  clean <- tibble::tibble()
  i <- 1
  for(phyto in c("prochloro","synecho","picoeuk","croco")){
    p <- stat %>%
          dplyr::filter(flag == 0, quantile == 2.5, pop == phyto) %>%
          dplyr::select(time,lat,lon, pop, abundance, contains(c(paste0(n[i],"_med"),paste0(n[i],"_mean")))) %>%
          dplyr::rename(diam = contains(paste0("diam_",n[i],"_med")), Qc = contains(paste0("Qc_",n[i],"_med"))) %>%
          dplyr::mutate(biomass = abundance * Qc) %>%
          dplyr::select(!ends_with("mean"))
    clean <- clean %>% dplyr::bind_rows(p)
    i <- i + 1
  }

  return(clean)

}


#' Clean the Particle size disitrubution by selecting the closest refractive index to the reference diameter for a particular population,
#' keep only the clean data (flag ==0 ) 
#'
#' @param PSD Particle size disitribution created by create_PSD().
#' @param pop Name of the population. Can be either "prochloro", "synecho", "picoeuk", "croco"
#' @param ref_diam Diameter value (in micron)
#' @return A cleaned Particle Size distribution with corrected refractive index
#' @export
#' 
get_clean_PSD <- function(PSD, pop="prochloro", ref_diam=0.54){

  ref <- as.numeric(ref_diam)
  phyto <- as.character(pop)

  ### Select the appropriate refractive index for each population
  ri <- tibble::tibble(lwr=1.055, mid=1.032, upr=1.017)

  # Select the appropriate refractive index 
  pre.dist <- transform_PSD(PSD, time.step=NULL, Qc.to.diam=TRUE, interval.to.geomean=TRUE, abundance.to.biomass=FALSE)
  pre.dist <- subset(pre.dist, flag==0)
  
  clmn <- grep("]", names(PSD)) # select column that contains the number of cells per diameters
  p_lwr <- colSums(pre.dist[pre.dist$pop==phyto & pre.dist$n == "lwr",clmn], na.rm=T)
    id_lwr <- which(p_lwr == max(p_lwr))
  p_mid <- colSums(pre.dist[pre.dist$pop==phyto & pre.dist$n == "mid",clmn], na.rm=T)
    id_mid <- which(p_mid == max(p_mid))
  p_upr <- colSums(pre.dist[pre.dist$pop==phyto & pre.dist$n == "upr",clmn], na.rm=T)
    id_upr <- which(p_upr == max(p_upr))
  
  diam <- as.numeric(colnames(pre.dist[,clmn]))
  diff <- abs(ref - diam[c(id_lwr, id_mid, id_upr)])
  best <- which(diff == min(diff))
  refrac <- c("lwr","mid","upr")[best[1]]

  if(is.na(refrac)) refrac <- "mid" # case when there is no population of interest

  print(paste0("best refractive index for ",phyto," : ",ri[refrac] ," (",refrac, ")"))
 
  ## Apply best refractive index for each population
  # High refractive index for large cells (Qc_lwr, diam_lwr)
  distribution <- PSD %>%
          dplyr::filter(pop == "prochloro" & n == refrac | 
                pop == "synecho" & n == refrac | 
                pop == "picoeuk" & n == "lwr" | 
                pop == "croco" & n == "lwr") %>%
          dplyr::filter(flag ==0) %>%
          dplyr::select(!c(n, opp_evt_ratio, flag))

  return(distribution)
}