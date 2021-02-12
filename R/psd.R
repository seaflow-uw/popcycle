#' Create particle size distributions as cell abundance for many files in VCT directories
#'
#' @param dbs Vector of SQLite3 database file paths.
#' @param vct_dirs Vector of VCT file directory paths, matching dbs.
#' @param data_col VCT column to use as size metric: fsc_small, chl_small, pe, Qc, or diam. If
#'  Qc or diam is selected, the refractive index is indicated in the extra column "n".
#' @param quantile OPP Filtering quantile.
#' @param bins Number of bins in distribution.
#' @param minval Value of smallest bin. Exclusive so set lower than lowest expected value.
#' @param maxval Value of largest bin. Inclusive. To account for floating point imprecision
#'   a small value of maxval_headroom will be added to maxval automatically.
#' @param maxval_headroom Amount to add to maxval to account for floating point imprecision.
#' @param log_base Log base to use when creating log-spaced bins for distribution.
#' @param cores Number of worker processes to create.
#' @return List of size distribution dataframes, named for their VCT directory.
#' @examples
#' \dontrun{
#' psds <- create_PSD(dbs, vct_dirs, "fsc_small", quantile = 50, bins = 40, minval = 1, maxval = 3500)
#' }
#' @export
create_PSD <- function(dbs, vct_dirs, data_col, quantile, bins, minval, maxval, maxval_headroom = 1e-6, log_base = 2, cores = 1) {
  ptm <- proc.time()

  if (length(dbs) != length(vct_dirs)) {
    stop("number of DB files not equalt to number of VCT directories")
  }
  vct_dirs <- normalizePath(vct_dirs)  # prevent a lot of trouble later
  vct_files <- purrr::flatten_chr(purrr::map(vct_dirs, ~ list.files(., "\\.parquet$", full.names=T)))

  cores <- min(cores, parallel::detectCores(), length(vct_files))
  cores <- max(cores, 1)

  # Check data column requested for validity and for presence of multiple refractive indexes
  refractive_cols <- c("Qc", "diam")
  channel_cols <- c("fsc_small", "chl_small", "pe")
  if (! (data_col %in% c(refractive_cols, channel_cols))) {
    stop(paste("data_col must be one of", paste(channel_cols, collapse=" "), paste(refractive_cols, collapse=" ")))
  }
  refractive <- data_col %in% refractive_cols
  QUANT <- as.numeric(quantile)

  ### Create PSD for each timepoint 
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    answer <- foreach::`%dopar%`(
      foreach::foreach(
        vct_file=vct_files,
        .inorder=TRUE,
        .packages=c("magrittr"),
        .export=c("create_PSD_one_file")
      ),
      create_PSD_one_file(vct_file, data_col, quantile, bins, minval, maxval, maxval_headroom, log_base)
    )
    parallel::stopCluster(cl)
  } else {
    # Serial code
    answer <- purrr::map(vct_files, ~ create_PSD_one_file(., data_col, quantile, bins, minval, maxval, maxval_headroom, log_base))
  }
  psds <- dplyr::bind_rows(answer)

  ### Calculate cell abundance
  PSDs <- list()
  for (i in seq_along(dbs)) {
    db_file <- dbs[[i]]
    vct_dir <- vct_dirs[[i]]

    # Subset distribution down to a single directory/db
    distribution <- psds %>% dplyr::filter(dir == vct_dir)

    # Final double check that the vct_dir and dir from dataframe match up
    dir1_from_df <- unique(as.vector(distribution$dir))
    dir2_from_df <- unique(dirname(as.vector(distribution$file)))
    if (dir1_from_df != vct_dir || dir2_from_df != vct_dir) {
      stop(paste("VCT directories and files became misaligned during PSD calculation. ", vct_dir, dir1_from_df, dir2_from_df))
    }
    # Now safe to remove file and dir columns
    distribution <- distribution %>%  dplyr::select(-c(file, dir))

    ### Calculate cell abundance in each bin (cells / microliter)
    PSDs[[i]] <- psd_abundance(db_file, distribution, quantile)
  }

  # Name the list with VCT directory
  names(PSDs) <- vct_dirs

  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")
  return(PSDs)
}

#' Create particle size distribution as raw counts for one VCT file
#'
#' @param vct_file VCT file path.
#' @param data_col VCT column to use as size metric: fsc_small, chl_small, pe, Qc, or diam. If
#'  Qc or diam is selected, the refractive index is indicated in the extra column "n".
#' @param quantile OPP Filtering quantile.
#' @param bins Number of bins in distribution.
#' @param minval Value of smallest bin. Exclusive so set lower than lowest expected value.
#' @param maxval Value of largest bin. Inclusive. To account for floating point imprecision
#'   a small value of maxval_headroom will be added to maxval automatically.
#' @param maxval_headroom Amount to add to maxval to account for floating point imprecision.
#' @param log_base Log base to use when creating log-spaced bins for distribution.
#' @return Size distribution dataframe.
#' @examples
#' \dontrun{
#' psd <- create_PSD_one_file(vct_file, "fsc_small", quantile = 50, bins = 40, minval = 1, maxval = 3500)
#' }
#' @export
create_PSD_one_file <- function(vct_file, data_col, quantile, bins, minval, maxval, maxval_headroom = 1e-6, log_base = 2) {
  # Check data column requested for validity and for presence of multiple refractive indexes
  refractive_cols <- c("Qc", "diam")
  channel_cols <- c("fsc_small", "chl_small", "pe")

  if (! (data_col %in% c(refractive_cols, channel_cols))) {
    stop(paste("data_col must be one of", paste(channel_cols, collapse=" "), paste(refractive_cols, collapse=" ")))
  }
  refractive <- data_col %in% refractive_cols

  breaks <- create_breaks(bins, minval, maxval + maxval_headroom, log_base)

  QUANT <- as.numeric(quantile)
  qstr <- paste0("q", QUANT)
  qsuffix <- paste0("_", qstr)

  if (refractive) {
      vct <- arrow::read_parquet(
        vct_file,
        col_select=c("date", all_of(qstr), c(starts_with(data_col) & ends_with(qsuffix)), paste0("pop", qsuffix))
      )
    } else {
      vct <- arrow::read_parquet(vct_file, col_select=c("date", all_of(qstr), all_of(data_col), paste0("pop", qsuffix)))
    }
    # Filter for only one quantile
    vct <- dplyr::filter(vct, get(qstr))
    # Remove any quantile suffixes from column names
    vct <- dplyr::rename_with(
      vct,
      function(x) { stringr::str_replace(x, paste0(qsuffix, "$"), "") },
      ends_with(qsuffix)
    )

    ## Get particle count in each bin 
    # group by population and by breaks
    # for each refractive index
    if (refractive) {
      psds <- list()
      for (refractive_level in c("lwr", "mid", "upr")) {
        refrac_col <- paste0(data_col, "_", refractive_level)
        col_sym <- dplyr::sym(refrac_col)
        refrac_psd <- vct %>% 
          dplyr::group_by(date, pop, breaks=cut(!!col_sym, breaks), .drop=F) %>%
          dplyr::count(breaks)
        # Check that no values are outside of breaks range. This will show up as a
        # break value of NA
        if (any(is.na(refrac_psd$breaks))) {
          stop(paste("data detected outside breaks range in file =", vct_file, "column =", refrac_col, "quantile=", QUANT))
        }
        refrac_psd <- refrac_psd %>%
          tidyr::pivot_wider(names_from = breaks, values_from = n) %>%
          tibble::add_column(n=refractive_level, .after="pop")
        psds[[length(psds)+1]] <- refrac_psd
      }
      # add data of each refractive index
      psd <- dplyr::bind_rows(psds)
    } else {
      # This data column doesn't have multiple refractive indexes
      col_sym <- dplyr::sym(data_col)
      psd <- vct %>% 
        dplyr::group_by(date, pop, breaks=cut(!!col_sym, breaks), .drop=F) %>%
        dplyr::count(breaks)
      # Check that no values are outside of breaks range. This will show up as a
      # break value of NA
      if (any(is.na(psd$breaks))) {
        stop(paste("data detected outside breaks range in file =", vct_file, "column =", data_col, "quantile=", QUANT))
      }
      psd <- psd %>% tidyr::pivot_wider(names_from = breaks, values_from = n)
    }
    # Add VCT file and directory to dataframe
    # Remove grouping
    psd <- psd %>%
      tibble::add_column(file=as.factor(vct_file), dir=as.factor(dirname(vct_file)), .before=1) %>%
      dplyr::ungroup()

    return(psd)
}

#' Find min/max for a data column / quantile pair in many VCT directories
#'
#' @param vct_dirs Vector of VCT file directory paths.
#' @param data_col VCT column to use as size metric: fsc_small, chl_small, pe, Qc, or diam. If
#'  Qc or diam is selected, the refractive index is indicated in the extra column "n".
#' @param quantile OPP Filtering quantile.
#' @param cores Number of worker processes to create.
#' @return Two item numeric vector of c(min_val, max_val)
#' @examples
#' \dontrun{
#' minmax <- get_vct_range(vct_dirs, "fsc_small", quantile = 50)
#' }
#' @export
get_vct_range <- function(vct_dirs, data_col, quantile, cores = 1)  {
  ptm <- proc.time()

  cores <- min(cores, parallel::detectCores())

  # TODO: incorporate outlier files

  # Get vector of file paths
  vct_files <- purrr::flatten_chr(purrr::map(vct_dirs, ~ list.files(., "\\.parquet$", full.names=T)))

  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    answer <- foreach::`%dopar%`(
      foreach::foreach(
        vct_file=vct_files,
        .inorder=TRUE,
        .packages=c("magrittr"),
        .export=c("get_vct_range_one_file")
      ),
      get_vct_range_one_file(vct_file, data_col, quantile)
    )
    answer <- purrr::flatten_dbl(answer)
    parallel::stopCluster(cl)
  } else {
    # Serial code
    answer <- purrr::map(vct_files, ~ get_vct_range_one_file(., data_col, quantile))
    answer <- purrr::flatten_dbl(answer)
  }

  deltat <- proc.time() - ptm
  message("Analyzed ", length(vct_files), " files in ", deltat[["elapsed"]], " seconds")

  return(c(min(answer), max(answer)))
}

#' Find min/max for a data column / quantile pair in one VCT file
#'
#' @param vct_file VCT file path.
#' @param data_col VCT column to use as size metric: fsc_small, chl_small, pe, Qc, or diam. If
#'  Qc or diam is selected, the refractive index is indicated in the extra column "n".
#' @param quantile OPP Filtering quantile.
#' @return Two item numeric vector of c(min_val, max_val)
#' @examples
#' \dontrun{
#' minmax <- get_vct_range_one_file(vct_file, "fsc_small", quantile = 50)
#' }
#' @export
get_vct_range_one_file <- function(vct_file, data_col, quantile) {
  # Check data column requested for validity and for presence of multiple refractive indexes
  refractive_cols <- c("Qc", "diam")
  channel_cols <- c("fsc_small", "chl_small", "pe")
  
  if (! (data_col %in% c(refractive_cols, channel_cols))) {
    stop(paste("data_col must be one of", paste(channel_cols, collapse=" "), paste(refractive_cols, collapse=" ")))
  }
  refractive <- data_col %in% refractive_cols

  qstr <- paste0("q", as.numeric(quantile))
  qsuffix <- paste0("_", qstr)

  # Get date, quantile boolean column, data columns, and pop columns from file
  if (refractive) {
    vct <- arrow::read_parquet(
      vct_file,
      col_select=c(all_of(qstr), c(starts_with(data_col) & ends_with(qsuffix)))
    )
  } else {
    vct <- arrow::read_parquet(vct_file, col_select=c(all_of(qstr), all_of(data_col)))
  }
  # Find min and max for each data column
  minmax <- vct %>%
    dplyr::filter(get(qstr)) %>%
    dplyr::summarise(dplyr::across(starts_with(data_col), ~ range(.x)))
  return(c(min(as.matrix(minmax)), max(as.matrix(minmax))))
}

#' Add metadata to a particle size distribution and convert raw counts to abundance
#'
#' @param db Popcycle database file path
#' @param distribution Distribution dataframe created by particle_counts_one_file()
#' @param quantile OPP Filtering quantile.
#' @return distribution with counts converted to abundance, opp and sfl metadata merged.
#' @examples
#' \dontrun{
#' psd <- psd_abundance(db, psd, 2.5)
#' }
#' @export
psd_abundance <- function(db, distribution, quantile) {
  QUANT <- as.numeric(quantile)

  ### Retrieve metadata
  ## Retrieve SFL table
  sfl <- get.sfl.table(db)
  # format time
  sfl$time <- as.POSIXct(sfl$date, format="%FT%T", tz="UTC")
  # retrieve flow rate (mL min-1) of detectable volume
  fr <- flowrate(sfl$stream_pressure, inst= get.inst(db))$flow_rate
  # convert to microL min-1
  fr <- fr * 1000
  # acquisition time (min)
  acq.time <- sfl$file_duration/60
  # volume in microL
  sfl$volume <- round(fr * acq.time , 0)
    
  ## Retrive Outlier table
  outliers <- get.outlier.table(db)
  # merge with sfl
  sfl.all <- merge(sfl, outliers, by="file")

  ## Retrive OPP table
  # retrieve opp/evt
  opp <- tibble::as_tibble(get.opp.table(db))
  opp <- opp %>% dplyr::filter(quantile == QUANT)

  ## merge all metadata
  meta <- tibble::as_tibble(merge(sfl.all, opp, by="file")[c("time", "lat","lon","volume","opp_evt_ratio","flag")])

  PSD <- tibble::as_tibble(merge(distribution, meta, by.x="date",by.y="time",all.x=T)) %>%
    dplyr::relocate(contains("]"), .after=flag) # reorder column

  ### Calculate cell abundance in each bin (cells / microliter)
  ## calculate the volume of virtual core for each population, 
  # volume of SeaFlow's virtual core is calculated based on a median value of opp_evt ratio for the entire cruise
  # except for small particles (i.e prochloro and synecho) where it is calcualted based on the opp_evt_ratio at that time
  id <- which(PSD$pop == "prochloro" | PSD$pop == "synecho")
  PSD[id, "volume"] <- PSD[id, "volume"] * PSD[id,"opp_evt_ratio"]
  PSD[-c(id), "volume"] <- PSD[-c(id), "volume"] * median(PSD[["opp_evt_ratio"]][-c(id)])
  
  ## calculate cell Abundance (cells uL-1) i.e normalize count by volume of SeaFlow's virtual core.
  clmn <- grep("]", names(PSD))
  PSD[,clmn] <- PSD[,clmn] / PSD[["volume"]]

  return(PSD)
}

#' Create log-spaced breaks vector that can be used with cut().
#'
#' @param bins Number of bins in distribution.
#' @param minval Value for the minimum break.
#' @param maxval Value for the maximum break.
#' @param log_base Log base to use when creating log-spaced bins for distribution.
#' @return Vector defining breaks for use with cut().
#' @examples
#' \dontrun{
#' breaks <- create_breaks(40, 1, 3500, 10)
#' }
#' @export
create_breaks <- function(bins, minval, maxval, log_base=2) {
  delta <- log(maxval/minval, base=log_base) / (bins - 1)
  breaks <- minval * log_base^((0:(bins-1)) * delta)
  return(breaks)
}
