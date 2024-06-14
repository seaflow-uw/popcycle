#' Define polygons for population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para_x Channel to use as x axis.
#' @param para_y Channel to use as y axis.
#' @param poly_log Named list of gating polygon definitions. If a definition for
#'   popname already exists it will be updated. If it doesn't exist it will be
#'   appended to the end to the list. If poly_log is NULL a new list will be
#'   created.
#' @return Version of poly.log with a new polygon defintion for popname.
#' @export
set_gating_params <- function(opp, popname, para_x, para_y, poly_log=NULL) {
  popname <- as.character(popname)
  para_x <- as.character(para_x)
  para_y <- as.character(para_y)

  par(mfrow=c(1,1))
  plot_cyt(opp, para_x, para_y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- splancs::getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para_x, para_y)

  poly_l <- list(poly)
  names(poly_l) <- popname

  if (is.null(poly_log)) {
    # Start a new gating entry
    poly_log <- poly_l
  } else {
    # if gate parameters for the same population already exist, overwrite,
    # otherwise append gate parameters for new population
    poly_log[popname] <- poly_l
  }
  return(poly_log)
}

#' Define polygons for manual population gating.
#'
#' @param opp OPP data frame.
#' @param popname Population name.
#' @param para_x Channel to use as x axis.
#' @param para_y Channel to use as y axis.
#' @param gates.log Per population named list of classification parameters. If
#'   a definition for popname already exists it will be updated. If it doesn't
#'   exist it will be appended to the end to the list. If NULL, a new list will
#'   be created.
#' @return gates.log with a new polygon defintion for popname.
#' @export
add_manual_classification <- function(opp, popname, para_x, para_y, gates.log = NULL) {
  popname <- as.character(popname)
  para_x <- as.character(para_x)
  para_y <- as.character(para_y)

  par(mfrow=c(1,1))
  plot_cyt(opp, para_x, para_y)
  mtext(paste("Set Gate for:",popname), font=2)
  poly <- splancs::getpoly(quiet=TRUE) # Draw Gate
  colnames(poly) <- c(para_x, para_y)

  poly_l <- list(method="manual", poly=poly)

  if (is.null(gates.log)) {
    # Start a new gating entry
    gates.log <- list()

  }
  gates.log[[popname]] <- poly_l
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
#' @param min_pe Only consider particles with pe > min.pe
#' @param gates.log Per population named list of classification parameters. If
#'   a definition for popname already exists it will be updated. If it doesn't
#'   exist it will be appended to the end to the list. If NULL, a new list will
#'   be created.
#' @return gates.log with a new FlowDensity func call parameter set for popname
#' @export
add_auto_classification <- function(popname, para_x, para_y, position, gates,
                                    scale, min_pe=NA, gates.log = NULL) {
  popname <- as.character(popname)
  para_x <- as.character(para_x)
  para_y <- as.character(para_y)

  # Named list of important function parameters for FlowDensity
  params <- list(
    method="auto",
    x=para_x,
    y=para_y,
    position=position,
    gates=gates,
    scale=scale,
    min.pe=min_pe
  )

  if (is.null(gates.log)) {
    # Start a new gating entry
    gates.log <- list()
  }
  gates.log[[popname]] <- params

  return(gates.log)
}

#' Classify particles based on manually defined population gates.
#'
#' @param opp OPP data frame.
#' @param params Named list of gating parameters. Must contain a params$poly
#'   entry with polygon definitions.
#' @param popname Name of the population
#' @return List of per particle classifications.
#' @export
manual_classify <- function(opp, params, popname) {
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
#' @export
auto_classify <- function(opp, params, popname) {
  if (is.null(opp$pop)) {
    opp$pop <- "unknown"
  }

  if (is.null(params)) {
    stop(paste0("No gate parameters found for ", popname))
  }

  # Only keep selected unknown pop rows and remove pop column
  if (is.null(params$min.pe) | is.na(params$min.pe)) {
   row.selection <- opp$pop == "unknown"
  } else {
   row.selection <- opp$pop == "unknown" & opp[,paste(params$x)] > params$min.pe
  }

  x <- opp[row.selection, sapply(opp, is.numeric)]
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

#' Classify particles from an OPP data frame.
#'
#' Classify particles from an OPP data frame using a gating scheme provided by gates.log.
#'
#' @param opp Data frame of linearized filtered particles.
#' @param gates.log A gating scheme from the function "add_manual_classification()"
#'   or "add_auto_classification()"
#' @return List of per particle classifications
#' @export
classify_opp <- function(opp, gates.log) {
  was_tibble <- FALSE
  if ("tbl_df" %in% class(opp)) {
    opp <- as.data.frame(opp)
  }
  for (popname in names(gates.log)) {
    params <- gates.log[[popname]]
    if (params$method == "manual") {
      opp <- manual_classify(opp, params, popname)
    } else if (params$method == "auto") {
      opp <- auto_classify(opp, params, popname)
    } else {
      stop(paste("unrecognized classification method in classify_opp", params$method))
    }
  }
  if (! is.null(opp$pop)) {
    opp$pop <- factor(opp$pop)
  }
  if (was_tibble) {
    opp <- tibble::as_tibble(opp)
  }
  return(opp)
}

#' Classify particles for a list of OPP files.
#'
#' Classify a  list of OPP files. Save per file aggregate population statistics
#' to SQLite3 database and save particle population definitions to text files
#' in vct_dir.
#'
#' @param db SQLite3 database file path.
#' @param opp_dir OPP file directory.
#' @param opp_files List of OPP files to classify. Include julian day directory.
#'   If NULL then all OPP files (regardless of outlier status) will attempt to
#'   be classified.
#' @param vct_dir VCT file output directory.
#' @param gating_id Single gating ID to use for all files in opp_files.
#' @param mie_table Optionally provide the Mie theory table. If NULL,
#'   the installed Mie Theory lookup table will be used. This table should be
#'   identical to one produced by read_mie_csv().
#' @param cores Number of cores to use for gating
#' @return None
#' @export
classify_opp_files <- function(db, opp_dir, opp_files, vct_dir, gating_id = NULL,
                               mie_table = NULL, cores = 1) {
  ptm <- proc.time()

  # Normalize opp_dir to make sure all paths are comparable
  opp_dir <- normalizePath(opp_dir, mustWork=TRUE)

  # Read installed Mie theory table if not provided
  if (is.null(mie_table)) {
    mie_table <- read_mie_csv()
  }

  # Get gating IDs for each file_id, add dates, input file paths,
  # output file paths, etc. All information needed to perform gating should
  # be captured in plan
  plan <- create_full_gating_plan(opp_files, db, opp_dir, vct_dir, gating_id = gating_id)

  message(nrow(plan), " files to classify")

  if (nrow(plan) > 0) {
    # Get all gating params for easy lookup later without talking to the db every
    # file_id
    gating_params <- list()
    for (gid in unique(plan$gating_id)) {
      gating_params[[gid]] <- get_gating_params_by_id(db, gid)
    }

    # Perform classification, save new VCT Parquet files
    # Group by input time-windowed OPP Parquet file path. Classifcation will be
    # performed for all file IDs in one parquet file at a time.
    by_window_opp_path <- dplyr::group_by(plan, window_opp_path)
    cores <- min(cores, parallel::detectCores())
    message("using ", cores, " cores")
    if (cores > 1) {
      # Parallel code
      cl <- parallel::makeCluster(cores, outfile="")
      doParallel::registerDoParallel(cl)
      windows <- dplyr::group_split(by_window_opp_path)
      answer <- foreach::foreach(df = windows, .inorder = TRUE, .combine = dplyr::bind_rows) %dopar% {
        classify_window_opp(df, NULL, gating_params, mie = mie_table)
      }
      parallel::stopCluster(cl)
    } else {
      # Serial code
      answer <- dplyr::group_map(
        by_window_opp_path,
        classify_window_opp,
        gating_params,
        mie = mie_table,
        .keep = TRUE
      )
    }
    # Save stats to DB
    vct_stats_all <- dplyr::bind_rows(answer)
    save_vct_stats(db, vct_stats_all)
  }

  deltat <- proc.time() - ptm
  message("Classified ", nrow(plan), " files in ", deltat[["elapsed"]], " seconds")
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
    df <- dplyr::bind_rows(df, agg)
  }
  cols <- c(
    "file_id", "pop", "count",
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
# x is a dataframe created by create_full_gating_plan() that contains
# configuration data for processing a single time-windowed OPP file.
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
classify_window_opp <- function(x, y, gating_params, mie=NULL) {
  plan <- x
  stopifnot(length(unique(plan$window_opp_path)) == 1)
  window_opp_path <- plan$window_opp_path[1]
  window_vct_path <- plan$window_vct_path[1]
  window_opp_df <- arrow::read_parquet(window_opp_path)

  logtext <- paste0(lubridate::now("GMT"), " :: ", Sys.getpid(), " :: starting ", basename(window_opp_path))
  processed <- dplyr::group_map(
    dplyr::group_by(window_opp_df, file_id),
    classify_3min_opp,
    plan,
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
  for (quant in QUANTILES) {
    popcol <- paste0("pop_q", quant)
    window_vct_df[[popcol]] <- factor(window_vct_df[[popcol]], levels = POPNAMES)
  }
  # TODO: add a check for gating_id and filter_id in window_vct_df
  # and plan matching per file. Make sure there's only one
  # of each per file and they match.
  dir.create(dirname(window_vct_path), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(window_vct_path)) {
    # Read old data for this window and remove data for files we just classified
    old_vct_df <- arrow::read_parquet(window_vct_path) %>%
      dplyr::filter(!(file_id %in% plan$file_id))
    window_vct_df <- dplyr::bind_rows(
      window_vct_df,
      dplyr::anti_join(old_vct_df, window_vct_df, by="file_id")
    )
    # If there's no data in this window (maybe new filtering removed it),
    # remove empty file
    if (nrow(window_vct_df) == 0) {
      file.remove(window_vct_path)
    }
    rm(old_vct_df)
    gc()
  }
  # Sort by ascending date
  window_vct_df <- dplyr::arrange(window_vct_df, date)
  # Write new VCT parquet
  if (nrow(window_vct_df) > 0) {
    # Use a temp file to avoid partial writes
    tmpname <- mktempname(dirname(window_vct_path), basename(window_vct_path))
    arrow::write_parquet(window_vct_df, tmpname)
    file.rename(tmpname, window_vct_path)
  }
  # Prepare VCT stats dataframe
  vct_stats_df <- prep_vct_stats(window_vct_df)
  rm(window_vct_df)
  gc()

  logtext <- paste0(
    logtext,
    "\n",
    paste0(lubridate::now("GMT"), " :: ", Sys.getpid(), " :: finished ", basename(window_opp_path))
  )
  message(logtext)
  return(vct_stats_df)
}

classify_3min_opp <- function(x, y, plan, gating_params, mie = NULL) {
  opp <- x
  stopifnot(length(y$file_id) == 1)
  file_id <- as.character(y$file_id[1])
  plan <- plan[plan$file_id == file_id, ]
  if (nrow(plan) == 0) {
    return(tibble::tibble())  # filter out from returned list later
  }
  inst <- plan$inst[1]
  filter_id <- as.character(plan$filter_id[1])
  gating_id <- as.character(plan$gating_id[1])
  gp <- gating_params[[gating_id]]
  if (gp$id != gating_id) {
    # It's really important that this lookup of gating parameters is done
    # correctly, so double-check here.
    stop(paste("gating ID mismatch for", file_id, ":", gating_id, "!=", gp$id, ""))
  }
  filter_id_from_file <- as.character(unique(opp$filter_id))
  if (filter_id != filter_id_from_file) {
    # Quick sanity check that OPP we're using matches what we pulled from the db
    stop(paste("filter ID mismatch for", file_id, ":", filter_id, "!=", filter_id_from_file, ""))
  }
  for (quantile in popcycle:::QUANTILES) {
    channels <- c("fsc_small", "pe", "chl_small")
    qcol <- paste0("q", quantile)  # name of logical column for a single quantile
    beads_col <- paste0("beads_fsc_", quantile)  # name of quantile-specific beads_fsc from filtering params
    qopp <- opp[opp[[qcol]], channels]  # quantile-specific OPP data
    beads_fsc <- plan[[beads_col]][1]
    # First calculate diameter and carbon quota
    qopp <- size_carbon_conversion(qopp, beads_fsc = beads_fsc, inst = inst, mie = mie)
    # Then gate
    qopp <- classify_opp(qopp, gp$gates.log)
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
