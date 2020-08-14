#' Define polygons for population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param poly.log Named list of gating polygon definitions. If a definition for
#'   popname already exists it will be updated. If it doesn't exist it will be
#'   appended to the end to the list. If poly.log is NULL a new list will be
#'   created.
#' @return Version of poly.log with a new polygon defintion for popname.
#' @examples
#' \dontrun{
#' poly.log <- set.gating.params(opp, "beads", "fsc_small", "pe")
#' poly.log <- set.gating.params(opp, "prochloro", "fsc_small", "chl_small",
#'                               poly.log)
#' }
#' @export
set.gating.params <- function(opp, popname, para.x, para.y, poly.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  par(mfrow=c(1,1))
  plot_cyt(opp, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- splancs::getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para.x, para.y)

  poly.l <- list(poly)
  names(poly.l) <- popname

  if (is.null(poly.log)) {
    # Start a new gating entry
    poly.log <- poly.l
  } else {
    # if gate parameters for the same population already exist, overwrite,
    # otherwise append gate parameters for new population
    poly.log[popname] <- poly.l
  }
  return(poly.log)
}

#' Define polygons for manual population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param gates.log Per population named list of classification parameters. If
#'   a definition for popname already exists it will be updated. If it doesn't
#'   exist it will be appended to the end to the list. If NULL, a new list will
#'   be created.
#' @return gates.log with a new polygon defintion for popname.
#' @examples
#' \dontrun{
#' gates.log <- add.manual.classification(opp, "beads", "fsc_small", "pe")
#' gates.log <- add.manual.classification(opp, "prochloro", "fsc_small", "chl_small",
#'                                        gates.log)
#' }
#' @export
add.manual.classification <- function(opp, popname, para.x, para.y, gates.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  par(mfrow=c(1,1))
  plot_cyt(opp, para.x, para.y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- splancs::getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para.x, para.y)

  poly.l <- list(method="manual", poly=poly)

  if (is.null(gates.log)) {
    # Start a new gating entry
    gates.log <- list()

  }
  gates.log[[popname]] <- poly.l
  return(gates.log)
}

#' Define polygons for manual population gating.
#'
#' @param popname Population name.
#' @param para.x Channel to use as x axis.
#' @param para.y Channel to use as y axis.
#' @param position position parameter to FlowDensity
#' @param gates gates parameter to FlowDensity
#' @param scale scale parameter to FlowDensity
#' @param min.pe Only consider partciesl with pe > min.pe
#' @param gates.log Per population named list of classification parameters. If
#'   a definition for popname already exists it will be updated. If it doesn't
#'   exist it will be appended to the end to the list. If NULL, a new list will
#'   be created.
#' @return gates.log with a new FlowDensity func call parameter set for popname
#' @examples
#' \dontrun{
#' gates.log <- add.auto.classification("beads", "fsc_small", "pe",
#'                                      c(FALSE,TRUE), c(2.0,NA), 0.975)
#' gates.log <- add.auto.classification("prochloro", "fsc_small", "chl_small",
#'                                      c(FALSE,TRUE), c(2.0,NA), 0.975, gates.log)
#' }
#' @export
add.auto.classification <- function(popname, para.x, para.y, position, gates,
                                    scale, min.pe=NA, gates.log=NULL) {
  popname <- as.character(popname)
  para.x <- as.character(para.x)
  para.y <- as.character(para.y)

  # Named list of important function parameters for FlowDensity
  params <- list(
    method="auto",
    x=para.x,
    y=para.y,
    position=position,
    gates=gates,
    scale=scale,
    min.pe=min.pe
  )

  if (is.null(gates.log)) {
    # Start a new gating entry
    gates.log <- list()
  }
  gates.log[[popname]] <- params

  return(gates.log)
}

#' Import old popcycle gating logs.
#'
#' Convert a list of old popcycle CSV gating log files into a poly.log
#' data structure identical to that produced by set.gating.params().
#'
#' @param csv.files CSV log files created by previous versions of popcycle
#' @param poly.log Named list of gating polygon definitions. Existing population
#'   gates will be updated. otherwise new ones will be appended to the list. If
#'   poly.log is NULL a new list will be created.
#' @return poly.log gating polygon definitions.
#' @examples
#' \dontrun{
#' poly.log <- gating.csv.to.poly.log(gates.csv.files)
#' }
#' @export
gating.csv.to.poly.log <- function(csv.files, poly.log=NULL) {
  for (onefile in csv.files) {
    # Find pop name from file name
    parts <- strsplit(onefile, "_")[[1]]
    last <- parts[length(parts)]  # e.g. beads.csv
    popname <- substr(last, 1, nchar(last) - 4)

    csv.poly <- read.csv(onefile)
    csv.poly <- list(as.matrix(csv.poly))
    names(csv.poly) <-popname

    if (is.null(poly.log)) {
      # Start a new gating entry
      poly.log <- csv.poly
    } else {
      # if gate parameters for the same population already exist, overwrite,
      # otherwise append gate parameters for new population
      poly.log[popname] <- csv.poly
    }
  }
  return(poly.log)
}




#' Classify particles based on manually defined population gates.
#'
#' @param opp OPP data frame.
#' @param params Named list of gating parameters. Must contain a params$poly
#'   entry with polygon definitions.
#' @param popname Name of the population
#' @return List of per particle classifications.
#' @examples
#' \dontrun{
#' vct <- manual.classify(opp, gates.log, "beads")
#' }
#' @export
manual.classify <- function(opp, params, popname) {
  if (is.null(opp$pop)) {
    opp$pop <- "unknown"
  }

  if (is.null(params)) {
    stop(paste0("No gate parameters found for ", popname))
  }

  poly <- params$poly # Get gating polygon definition
  para <- colnames(poly)  # channels
  df <- opp[opp$pop=="unknown", para]

  if (nrow(df) > 0) {
    colnames(poly) <- colnames(df) <- c("x","y") # to stop stupid Warnings from splancs::inout()
    vct <- df[splancs::inout(df,poly=poly, bound=TRUE, quiet=TRUE), ] # subset particles based on Gate
    opp[row.names(vct), "pop"] <- popname
  }

  return(opp)
}


#' Classify particles based on semisupervized clustering method from flowDensity package.
#'
#' @param opp OPP data frame.
#' @param params Named list of gating parameters. Must contain a params$poly
#'   entry with polygon definitions.
#' @param popname Name of the population
#' @return List of per particle classifications.
#' @examples
#' \dontrun{
#' vct <- auto.classify(opp, params, popname)
#' }
#' @export
auto.classify <- function(opp, params, popname) {
  if (is.null(opp$pop)) {
    opp$pop <- "unknown"
  }

  if (is.null(params)) {
    stop(paste0("No gate parameters found for ", popname))
  }

  # Only keep selected unknow pop rows and remove pop column
  if (is.null(params$min.pe) | is.na(params$min.pe)) {
   row.selection = opp$pop == "unknown"
  } else {
   row.selection = opp$pop == "unknown" & opp[,paste(params$x)] > params$min.pe
  }

  x <- opp[row.selection, names(opp) != "pop"]
  # Sometimes there are no unknowns at this point so check for zero rows
  if (nrow(x) > 0) {
    fframe <- flowCore::flowFrame(as.matrix(log10(x)))
    #plotDens(f, channels=c(5,8))
    channels <- c(match(params$x, names(x)), match(params$y, names(x)))
    labeled <- flowDensity::flowDensity(obj=fframe,channels=channels,
                                        position=params$position,
                                        gates=params$gates, ellip.gate=TRUE,
                                        scale=params$scale)
    opp[row.names(x[labeled@index,]),"pop"] <- popname
  }

  return(opp)
}

#' Classify particles from an OPP dataframe.
#'
#' Classify particles from an OPP dataframe using a gating scheme provided by gates.log.
#'
#' @param opp SQLite3 database file path.
#' @param gates.log A gating scheme from the function "add.manual.classification()" or "add.auto.classification()"
#' @return List of per particle classifications
#' @examples
#' \dontrun{
#' opp <- classify.opp(opp, gates.log)
#' }
#' @export
classify.opp <- function(opp, gates.log) {
  for (popname in names(gates.log)) {
    params <- gates.log[[popname]]
    if (params$method == "manual") {
      opp <- manual.classify(opp, params, popname)
    } else if (params$method == "auto") {
      opp <- auto.classify(opp, params, popname)
    } else {
      stop(paste("unrecognized classification method in classify.opp", params$method))
    }
  }
  if (! is.null(opp$pop)) {
    opp$pop <- factor(opp$pop)
  }
  return(opp)
}

#' Classify particles for a list of OPP files.
#'
#' Classify a  list of OPP files. Save per file aggregate population statistics
#' to SQLite3 database and save particle population definitions to text files
#' in vct.dir.
#'
#' @param db SQLite3 database file path.
#' @param opp.dir OPP file directory.
#' @param opp.files List of OPP files to classify. Include julian day directory.
#' @param vct.dir VCT file output directory.
#' @param gating.id Optionally provide the ID for gating parameters. If NULL,
#'  vct.table will be used to determine gating parameters. If vct.table is null,
#'  the latest gating parameters in the database will be used. gating.id takes
#'  precedence over vct.table.
#' @param vct.table Optionally provide a previous VCT table that will be used to
#'   gating parameters by date range. If NULL, gating.id will be used for all
#'   files. If gating.id is NULL, the latest gating parameters in the database
#'   will be used.
#' @param mie.table Optionally provide the Mie theory table. If NULL,
#'   the installed Mie Theory lookup table will be used. This table should be
#'   identical to one produced by read_mie_csv().
#' @return None
#' @examples
#' \dontrun{
#' classify.opp.files(db, opp.dir, opp.files, vct.dir)
#' classify.opp.files(db, opp.dir, opp.files, vct.dir,
#'                    "d3afb1ea-ad20-46cf-866d-869300fe17f4")
#' }
#' @export
classify.opp.files <- function(db, opp.dir, opp.files, vct.dir,
                               gating.id=NULL, vct.table=NULL, mie.table=NULL,
                               cores=1) {
  ptm <- proc.time()

  # Read installed Mie theory table if not provided
  if (is.null(mie.table)) {
    mie.table <- read_mie_csv()
  }
  # Clean up opp.files to produce only file IDs
  file_ids_to_gate <- unlist(lapply(opp.files, clean.file.path))

  # Get gating IDs for each file_id, add dates, input file paths,
  # output file paths, etc. All information needed to perform gating should
  # be captured in gating_plan
  gating_plan <- create_gating_plan(
    file_ids_to_gate, db, opp.dir, vct.dir, gating_id=gating.id, vct_table=vct.table
  )
  # Get all gating params for easy lookup later without talking to the db every
  # file_id
  gating_params <- list()
  for (gid in unique(gating_plan$gating_id)) {
    gating_params[[gid]] <- get.gating.params.by.id(db, gid)
  }

  # Perform classification, save new VCT Parquet files
  # Group by input time-windowed OPP Parquet file path. Classifcation will be
  # performed for all file IDs in one parquet file at a time.
  by_window_opp_path <- dplyr::group_by(
    gating_plan,
    window_opp_path
  )
  cores <- min(cores, parallel::detectCores())
  print(paste0("using ", cores, " cores"))
  if (cores > 1) {
    # Parallel code
    cl <- parallel::makeCluster(cores, outfile="")
    doParallel::registerDoParallel(cl)
    answer <- foreach::`%dopar%`(
      foreach::foreach(
        df=dplyr::group_split(by_window_opp_path),
        .inorder=TRUE,
        .combine=dplyr::bind_rows
      ),
      handle_window_opp(df, NULL, gating_params, mie=mie.table)
    )
    parallel::stopCluster(cl)
  } else {
    # Serial code
    answer <- dplyr::group_map(
      by_window_opp_path,
      handle_window_opp,
      gating_params,
      mie=mie.table,
      .keep=TRUE
    )
  }
  # Save stats to DB
  vct_stats_all <- dplyr::bind_rows(answer)
  save.vct.stats(db, vct_stats_all)
  deltat <- proc.time() - ptm
  writeLines(paste0("Classified ", length(opp.files), " in ", deltat[["elapsed"]], " seconds"))
}

#' Get per file gating IDs to use for regating based on previous gating.
#'
#' Based on the time ranges different gating parameters were applied during
#' a previous gating run (as captured by vct_table), return a DataFrame with
#' OPP file to gating ID matches.
#'
#' @param db SQLite3 database file path.
#' @param opp_files List of OPP files to classify. Include julian day directory.
#' @param vct_table VCT table from past gating.
#' @return DataFrame of "file", "opp_dates", "gating_id"
#' @examples
#' \dontrun{
#' get.opp.gates(db, opp_files, vct_table)
#' }
#' @export
get.opp.gates <- function(db, opp_files, vct_table, verbose=TRUE) {
  # Get run length encoding results for gating ids. We're trying to find the
  # boundaries of different gating parameters throughout the cruise.
  # VCT table must be sorted by date to do this properly
  vct_table <- vct_table[order(vct_table$date), ]
  rle_result <- rle(vct_table$gating_id)
  gating_start_idx <- rle_starts(rle_result)  # start of each gating section

  # Get the first dates for each gating section
  gating_start_dates <- lubridate::ymd_hms(vct_table[gating_start_idx, "date"])

  if (verbose) {
    print("Timestamps during cruise where new gating parameters will be applied")
    tmpdf <- data.frame(
      date=gating_start_dates,
      gating_id=vct_table[gating_start_idx, "gating_id"]
    )
    print(tmpdf)
  }

  # Get files and dates
  opp <- get.opp.dates(db, opp_files)
  intervals <- findInterval(
    as.numeric(lubridate::ymd_hms(opp$date)),
    as.numeric(gating_start_dates)
  )

  # For values which are before any interval, findInterval returns an index of
  # 0. Convert those to 1 here, meaning use the first gating parameters for
  # these files.
  intervals <- sapply(intervals, function(x) {
    if (x == 0) { return(1) } else { return(x) }
  })

  gating_ids <- rle_result$values
  opp_gating_ids <- gating_ids[intervals]
  df <- data.frame(file=opp$file, opp_dates=opp$date, gating_id=opp_gating_ids)
  return(df)
}

#' Get start indexes for run length encoding results.
#'
#' @param rle_result Result from rle()
#' @return Integer vector
#' @examples
#' \dontrun{
#' starts <- rle_starts(rle(c(1,1,1,2,2,3,3)))
#' }
rle_starts <- function(rle_result) {
  i <- 1
  loc <- 1
  starts <- c()
  for (i in 1:length(rle_result$lengths)) {
    starts[i] <- loc
    loc <- loc + rle_result$lengths[i]
    i <- i + 1
  }
  return(starts)
}

copy_column_parts <- function(src, dst, dst_index) {
  src_types <- sapply(src, typeof)
  for (i in seq_along(src_types)) {
    col_name <- names(src_types)[i]
    col_type <- src_types[[i]]
    if (!(col_name %in% names(dst))) {
      # Initially set all values to NA with same type as src column
      # Fill in values at dst_index with real data from src
      if (col_type == "double") {
        dst[[col_name]] <- as.double(NA)
        dst[dst_index, col_name] <- src[[col_name]]
      }  else if (col_type == "integer") {
        if (class(src[[col_name]]) == "factor") {
          # Convert factor to character
          dst[[col_name]] <- as.character(NA)
          dst[dst_index, col_name] <- as.character(src[[col_name]])
        } else if (class(src[[col_name]]) == "integer") {
          dst[[col_name]] <- as.integer(NA)
          dst[dst_index, col_name] <- src[[col_name]]
        } else {
          stop(paste("unsupported column type used in copy_column_parts()", col_name, col_type))
        }
      } else if (col_type == "character") {
        dst[[col_name]] <- as.character(NA)
        dst[dst_index, col_name] <- src[[col_name]]
      } else {
        stop(paste("unsupported column type used in copy_column_parts()", col_name, col_type))
      }
    }
  }
  return(dst)
}


create_gating_plan <- function(file_ids_to_gate, db, opp_dir, vct_dir, gating_id=NULL, vct_table=NULL) {
  if (!is.null(gating_id) & !is.null(vct_table)) {
    stop("gating.id and vct.table are mutually exclusive parameters")
  }
  
  # Start with a dataframe of file_ids
  df <- tibble::tibble(file_id=file_ids_to_gate)
  # Add dates from DB and only keep files in DB based on row file_id
  df <- add_dates(df, db)
  # Add time-windowed OPP and VCT file paths and intervals based on row date
  df <- add_opp_vct_paths(df, opp_dir, vct_dir)
  # Add gating IDs based on row file_id or date
  df <- add_gating_ids(df, db, gating_id, vct_table)
  # Add filter_id and per-quantile fsc bead pos from filter parameters
  df <- add_filter_params(df, db)
  # Add instrument serial number
  df <- add_instrument_serial(df, db)
  
  return(df)
}

add_dates <- function(df, db) {
  # Add a "date" column based on the SFL db table to a dataframe of "file_id"
  # Get SFL dates
  dates <- get_file_dates(db, df$file_id)
  df$date <- dates$date
  return(df)
}

add_opp_vct_paths <- function(df, opp_dir, vct_dir) {
  # Add time windowed paths and intervals to a dataframe with column "date"
  # Get intervals covered by each windowed OPP file
  paths <- list.files(opp_dir, pattern=paste0("*.opp.parquet"), full.names=TRUE)
  # Add interval and window_path column to df based on date
  df <- add_window_paths(df, paths)
  df <- dplyr::rename(df, window_opp_path=window_path)  # rename to be opp specific
  # Add VCT output paths
  ext1 <- "opp.parquet"
  ext2 <- "vct.parquet"
  df <- dplyr::mutate(
    df,
    window_vct_path=file.path(
      vct_dir,
      stringr::str_replace(basename(window_opp_path), ext1, ext2)
    )
  )
  return(df)
}

add_gating_ids <- function(df, db, gating_id, vct_table) {
  # Add a "gating_id" column to a dataframe with a "file_id" column
  if (is.null(gating_id) & is.null(vct_table)) {
    # No guidance on what gating parameters to used has been supplied.
    # Default to using the latest gating parameters for all files
    gating_id <- get.gating.params.latest(db)$id
    df$gating_id <- gating_id
  } else if (!is.null(gating_id)) {
    # Use provided gating_id for all files
    df$gating_id <- gating_id
  } else if (!is.null(vct_table)) {
    # Get gates by date range based on vct_table parameter
    opp_gates <- get.opp.gates(db, df$file_id, vct_table)
    if (any(opp_gates$file != df$file_id)) {
      stop("opp file_id mismatch when determining gating IDs from VCT table")
    }
    df$gating_id <- as.character(opp_gates$gating_id)  # make sure it's not a factor
  }
  return(df)
}

add_filter_params <- function(df, db) {
  # Add "filter_id", "beads_fsc_2.5", "beads_fsc_50", "beads_fsc_97.5" columns
  # Assume that one set of filter parameters has been used for all rows in df
  fp <- get.filter.params.latest(db)
  df$filter_id <- fp$id[1]
  df$beads_fsc_2.5 <- transformData(data.frame(fsc=fp[fp$quantile == 2.5, "beads.fsc.small"]))$fsc[1]
  df$beads_fsc_50 <- transformData(data.frame(fsc=fp[fp$quantile == 50, "beads.fsc.small"]))$fsc[1]
  df$beads_fsc_97.5 <- transformData(data.frame(fsc=fp[fp$quantile == 97.5, "beads.fsc.small"]))$fsc[1]
  return(df)
}

add_instrument_serial <- function(df, db) {
  df$inst <- get.inst(db)
  return(df)
}

prep_vct_stats <- function(vct) {
  # Now group by file_id and pop and get summary statistics for all
  # quantiles.
  vct <- tibble::as_tibble(vct)
  df <- tibble::tibble()  # to store aggregated stats for all quantiles
  for (quant in popcycle:::QUANTILES) {
    qcol <- paste0("q", quant)
    qvct <- vct[vct[[qcol]], ]  # Only keep rows for this quantile
    # Remove quantile suffix for quantile-specific columns for this
    # quantile
    qsuffix <- paste0("_q", quant)
    qvct <- dplyr::rename_with(
      qvct,
      function(.x) {
        stringr::str_replace(.x, paste0(qsuffix, "$"), "")
      },
      ends_with(qsuffix)
    )
    agg <- dplyr::summarize(
      dplyr::group_by(qvct, file_id, pop),
      count=dplyr::n(),
      chl_1q=as.numeric(quantile(chl_small, 0.25)),
      chl_med=as.numeric(quantile(chl_small, 0.5)),
      chl_3q=as.numeric(quantile(chl_small, 0.75)),
      pe_1q=as.numeric(quantile(pe, 0.25)),
      pe_med=as.numeric(quantile(pe, 0.5)),
      pe_3q=as.numeric(quantile(pe, 0.75)),
      fsc_1q=as.numeric(quantile(fsc_small, 0.25)),
      fsc_med=as.numeric(quantile(fsc_small, 0.5)),
      fsc_3q=as.numeric(quantile(fsc_small, 0.75)),
      diam_lwr_1q=as.numeric(quantile(diam_lwr, 0.25)),
      diam_lwr_med=as.numeric(quantile(diam_lwr, 0.5)),
      diam_lwr_3q=as.numeric(quantile(diam_lwr, 0.75)),
      diam_mid_1q=as.numeric(quantile(diam_mid, 0.25)),
      diam_mid_med=as.numeric(quantile(diam_mid, 0.5)),
      diam_mid_3q=as.numeric(quantile(diam_mid, 0.75)),
      diam_upr_1q=as.numeric(quantile(diam_upr, 0.25)),
      diam_upr_med=as.numeric(quantile(diam_upr, 0.5)),
      diam_upr_3q=as.numeric(quantile(diam_upr, 0.75)),
      Qc_lwr_1q=as.numeric(quantile(Qc_lwr, 0.25)),
      Qc_lwr_med=as.numeric(quantile(Qc_lwr, 0.5)),
      Qc_lwr_mean=mean(Qc_lwr),
      Qc_lwr_3q=as.numeric(quantile(Qc_lwr, 0.75)),
      Qc_mid_1q=as.numeric(quantile(Qc_mid, 0.25)),
      Qc_mid_med=as.numeric(quantile(Qc_mid, 0.5)),
      Qc_mid_mean=mean(Qc_mid),
      Qc_mid_3q=as.numeric(quantile(Qc_mid, 0.75)),
      Qc_upr_1q=as.numeric(quantile(Qc_upr, 0.25)),
      Qc_upr_med=as.numeric(quantile(Qc_upr, 0.5)),
      Qc_upr_mean=mean(Qc_upr),
      Qc_upr_3q=as.numeric(quantile(Qc_upr, 0.75)),
      gating_id=dplyr::first(gating_id),
      filter_id=dplyr::first(filter_id),
      quantile=quant,
      .groups="drop"  # see https://www.tidyverse.org/blog/2020/05/dplyr-1-0-0-last-minute-additions/#summarise-and-grouping
    )
    agg <- dplyr::rename(agg, file=file_id)
    df <- dplyr::bind_rows(df, agg)
  }
  cols <- c(
    "file", "pop", "count",
    "chl_1q","chl_med", "chl_3q",
    "pe_1q","pe_med", "pe_3q",
    "fsc_1q","fsc_med", "fsc_3q",
    "diam_lwr_1q","diam_lwr_med","diam_lwr_3q",
    "diam_mid_1q","diam_mid_med","diam_mid_3q",
    "diam_upr_1q","diam_upr_med","diam_upr_3q",
    "Qc_lwr_1q","Qc_lwr_med","Qc_lwr_mean","Qc_lwr_3q",
    "Qc_mid_1q","Qc_mid_med","Qc_mid_mean","Qc_mid_3q",
    "Qc_upr_1q","Qc_upr_med","Qc_upr_mean","Qc_upr_3q",
    "gating_id", "filter_id", "quantile"
  )
  df_reorder <- df[cols]
  return(df_reorder)
}

# Process a single time-windowed OPP file.
# 
# x is a dataframe created by create_gating_plan() that relate to processing a
# single time-windowed OPP file.
# 
# y will be ignored, it is usually the group key (OPP file path) required by
# dplry::group_map but here we get the group key from the single unique value in
# x$window_opp_path.
# 
# gating_params is a named list that can be used to lookup gating parameters by
# gating ID string.
# 
# mie is optionally a Mie theory lookup table dataframe to be used during VCT
# creation.
handle_window_opp <- function(x, y, gating_params, mie=NULL) {
  gating_plan <- x
  # It should be a column in the dataframe x, probably used as the group by
  # key, therefore we can assume there is only one value.
  window_opp_path <- x$window_opp_path[1]
  window_vct_path <- gating_plan$window_vct_path[1]
  window_opp_df <- arrow::read_parquet(window_opp_path)

  by_file_id <- dplyr::group_by(window_opp_df, file_id)
  logtext <- paste0(lubridate::now("GMT"), " :: ", Sys.getpid(), " :: starting ", basename(window_opp_path))
  processed <- dplyr::group_map(
    by_file_id,
    handle_3min_opp,
    gating_plan,
    gating_params,
    mie,
    .keep=TRUE
  )
  rm(window_opp_df)
  gc()
  # Remove empty dataframes from processed
  processed <- Filter(function(df) {nrow(df) > 0}, processed)
  # Combine all 3 minute OPPs into one new time-windowed OPP
  window_vct_df <- dplyr::bind_rows(processed)
  rm(processed)
  gc()
  # Turn all pop columns into factors if not already
  window_vct_df <- dplyr::mutate_at(
    window_vct_df,
    dplyr::vars(starts_with("pop_q")),
    as.factor
  )
  # TODO: add a check for gating_id and filter_id in window_vct_df
  # and gating_plan matching per file. Make sure there's only one
  # of each per file and they match.
  dir.create(dirname(window_vct_path), showWarnings=FALSE, recursive=TRUE)
  if (file.exists(window_vct_path)) {
    old_vct_df <- arrow::read_parquet(window_vct_path)
    window_vct_df <- merge_old_new_vct(old_vct_df, window_vct_df)
    rm(old_vct_df)
    gc()
  }
  # Sort by ascending date
  window_vct_df <- dplyr::arrange(window_vct_df, date)
  arrow::write_parquet(window_vct_df, window_vct_path)
  # Prepare VCT stats dataframe
  vct_stats_df <- prep_vct_stats(window_vct_df)
  rm(window_vct_df)
  gc()
  # TODO: Read existing VCT window file
  # Merge new data into existing VCT window file
  logtext <- paste0(
    logtext,
    "\n",
    paste0(lubridate::now("GMT"), " :: ", Sys.getpid(), " :: finished ", basename(window_opp_path))
  )
  writeLines(logtext)
  return(vct_stats_df)
}

handle_3min_opp <- function(x, y, gating_plan, gating_params, mie=NULL) {
  file_id <- as.character(y$file_id[1])
  opp <- x
  gating_plan <- gating_plan[gating_plan$file_id == file_id, ]
  if (nrow(gating_plan) == 0) {
    return(tibble::tibble())  # filter out from returned list later
  }
  inst <- gating_plan$inst[1]  
  filter_id <- as.character(gating_plan$filter_id[1])
  gating_id <- as.character(gating_plan$gating_id[1])
  gp <- gating_params[[gating_id]]
  if (gp$id != gating_id) {
    # It's really important that this lookup of gating parameters is done
    # correctly, so double-check here.
    stop(paste("gating ID mismatch for", file_id, ":", gating_id, "!=", gp$id, ""))
  }
  filter_id_from_file <- unique(opp$filter_id)
  if (filter_id != filter_id_from_file) {
    # Quick sanity check that OPP we're using matches what we pulled from the db
    stop(paste("filter ID mismatch for", file_id, ":", filter_id, "!=", filter_id_from_file, ""))
  }
  for (quantile in popcycle:::QUANTILES) {
    channels <- c("fsc_small", "pe", "chl_small")
    qcol <- paste0("q", quantile)  # name of logical column for a single quantile
    beads_col <- paste0("beads_fsc_", quantile)  # name of quantile-specific beads_fsc from filtering params
    qopp <- opp[opp[[qcol]], channels]  # quantile-specific OPP data
    beads_fsc <- gating_plan[[beads_col]][1]
    qopp <- as.data.frame(qopp)  # something in carbon conversion of classify.opp doesn't like tibbles
    # First calculate diameter and carbon quota
    qopp <- size.carbon.conversion(qopp, beads.fsc=beads_fsc, inst=inst, mie=mie)
    # Then gate
    qopp <- classify.opp(qopp, gp$gates.log)
    # Select and rename quantile-specific columns. No need to keep the 
    # OPP channel data, that's still in the original OPP dataframe.
    # Add "_q<quantile>" to each new column to indicate it's quantile.
    qopp <- dplyr::rename_with(
      dplyr::select(qopp, !c(fsc_small, pe, chl_small)),
      ~ stringr::str_c(.x, paste0("_q", quantile))
    )
    # Copy new columns for this quantile's subset of rows back into opp
    opp <- copy_column_parts(qopp, opp, opp[[qcol]])
  }
  opp <- tibble::as_tibble(opp)
  # Add gating ID here as factor for easier record keeping
  # downstream, and to make sure the correct gating parameters were used
  # for this file. Note the gating ID is captured from the actual gating
  # parameters used, not just the gating ID that was passed into this
  # function.
  opp$gating_id <- as.factor(gp$id)
  return(opp)
}


#' Merge two VCT dataframes by adding rows for novel file_id in old into new.
#' @param old_vct_df VCT to pull novel file_id entries from.
#' @param new_vct_df VCT into which novel file_id entries will be added.
#' @return new_vct_df with novel file_id entries from old_vct_df.
merge_old_new_vct <- function(old_vct_df, new_vct_df) {
  merged <- dplyr::bind_rows(
    new_vct_df,
    dplyr::anti_join(old_vct_df, new_vct_df, by="file_id")
  )
  return(merged)
}
