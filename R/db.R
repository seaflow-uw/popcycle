#' Delete DB filter parameters by ID.
#'
#' @param db SQLite3 database file path.
#' @param filter_id ID for filter entries.
#' @return Number of rows deleted.
#' @export
delete_filter_params_by_id <- function(db, filter_id) {
  sql <- paste0("DELETE FROM filter WHERE id == '", filter_id, "'")
  sql_dbExecute(db, sql)
}

#' Delete DB gating parameters by gating ID.
#'
#' Any gating polygon entries in the poly table will also be deleted.
#'
#' @param db SQLite3 database file path.
#' @param gating_id ID for gating and poly entries.
#' @return Number of rows deleted.
#' @export
delete_gating_params_by_id <- function(db, gating_id) {
  sql <- paste0("DELETE FROM gating WHERE id == '", gating_id, "'")
  sql_dbExecute(db, sql)
  delete_poly_by_id(db, gating_id)
}

#' Delete DB poly parameters by gating ID.
#'
#' Note: This is usually done via delete.gating.params.by.id.
#'
#' @param db SQLite3 database file path.
#' @param gating_id gating_id for poly entries.
#' @return Number of rows deleted.
delete_poly_by_id <- function(db, gating_id) {
  sql <- paste0("DELETE FROM poly WHERE gating_id == '", gating_id, "'")
  sql_dbExecute(db, sql)
}

#' Delete DB poly parameters by gating ID and population
#'
#' Note: This is usually done via delete.gating.params.by.id.
#'
#' @param db SQLite3 database file path.
#' @param gating_id gating_id for poly entries.
#' @param popname Population name
#' @return Number of rows deleted.
delete_poly_by_id_pop <- function(db, gating_id, popname) {
  sql <- paste0("DELETE FROM poly WHERE gating_id == '", gating_id, "' AND pop == '", popname, "'")
  sql_dbExecute(db, sql)
}

#' Delete all rows in opp table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_opp_table <- function(db) {
  reset_table(db, "opp")
}

#' Delete all rows in vct table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_vct_table <- function(db) {
  reset_table(db, "vct")
}

#' Delete all rows in sfl table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_sfl_table <- function(db) {
  reset_table(db, "sfl")
}

#' Delete all rows in filter table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_filter_table <- function(db) {
  reset_table(db, "filter")
}

#' Delete all rows in gating table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_gating_table <- function(db) {
  reset_table(db, "gating")
}

#' Delete all rows in poly table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_poly_table <- function(db) {
  reset_table(db, "poly")
}

#' Delete all rows in gating plan table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_gating_plan_table <- function(db) {
  reset_table(db, "gating_plan")
}

#' Delete all rows in filter plan table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_filter_plan_table <- function(db) {
  reset_table(db, "filter_plan")
}

#' Delete all rows in outlier table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_outlier_table <- function(db) {
  reset_table(db, "outlier")
}

#' Delete all rows in metadata table.
#'
#' @param db SQLite3 database file path.
#' @return Number of rows deleted.
#' @export
reset_metadata_table <- function(db) {
  reset_table(db, "metadata")
}

#' Delete all rows in an arbitrary SQLite3 DB table.
#'
#' @param db SQLite3 database file path.
#' @param table_name Table name.
#' @return Number of rows deleted.
reset_table <- function(db, table_name) {
  sql <- paste0("DELETE FROM ", table_name)
  sql_dbExecute(db, sql)
}

#' Get filter parameters by id.
#'
#' @param db SQLite3 database file path.
#' @param filter_id ID for entry in filter table.
#' @return Data frame of filter parameters matchign filter_id.
#' @export
get_filter_params_by_id <- function(db, filter_id) {
  sql <- paste0("SELECT * FROM filter WHERE id = '", filter_id, "' ORDER BY quantile")
  result <- sql_dbGetQuery(db, sql)
  if (nrow(result) == 0) {
    stop("no filter parameters found for ", filter_id)
  }
  # DB column names have underscores due to sqlite column naming restrictions.
  # To get this dataframe to match filter parameter column names used
  # elsewhere in this code base and to match R variable naming style
  # conventions, switch "_" for "." in all column names.
  names(result) <- unlist(lapply(names(result), function(n) {
    return(gsub("_", ".", n, fixed=T))
  }))
  return(result)
}

#' Get gating parameters by ID.
#'
#' @param db SQLite3 database file path.
#' @param gating_id ID in gating table and poly table.
#' @return Named list where list$gates.log a recreation of original gating
#'   created by add_manual_classification(), add_auto_classification(), and
#'   save_gating_params()
#' @export
get_gating_params_by_id <- function(db, gating_id) {
  sql <- paste0("SELECT * FROM gating WHERE id = '", gating_id, "' ORDER BY pop_order ASC")
  gating.df <- sql_dbGetQuery(db, sql)
  if (nrow(gating.df) == 0) {
    stop(paste0("No entry found in gating table for ", gating_id))
  }

  gates.log <- list()

  for (i in seq(nrow(gating.df))) {
    r <- gating.df[i, ]
    if (r$method == "manual") {
      poly.log <- get_poly_log_by_gating_id_pop(db, gating_id, r$pop)
      gates.log[[r$pop]] <- list(method=r$method, poly=poly.log)
    } else if (r$method == "auto") {
      gates.log[[r$pop]] <- list(
        method=r$method,
        x=r$channel1,
        y=r$channel2,
        position=c(r$position1 == 1, r$position2 == 1),  # coerce to boolean from 1,0 integer stored in db
        gates=c(r$gate1, r$gate2),
        scale=r$scale,
        min.pe=r$minpe
      )
    } else {
      stop(paste0("unrecognized classification method ", r$method));
    }
  }

  answer <- list(id = gating.df[1, "id"], gates.log = gates.log)
  return(answer)
}

#' Construct a gating polygon list for gating_id pop combo
#'
#' @param db SQLite3 database file path.
#' @param gating_id Foreign key to gating table.
#' @param popname Population name
#' @return List of population gating polygon coordinates.
get_poly_log_by_gating_id_pop <- function(db, gating_id, popname) {
  poly.log <- list()
  sql <- paste0("
    SELECT * FROM poly
    WHERE
      gating_id = '", gating_id, "'
      AND
      pop = '", popname, "'
    ORDER BY point_order"
  )
  pop.poly <- sql_dbGetQuery(db, sql)

  for (c in EVT.HEADER[5:length(EVT.HEADER)]) {
    if (c %in% colnames(pop.poly)) {
      if (all(is.na(pop.poly[, c]))) {
        pop.poly[, c] <- NULL
      }
    }
  }
  pop.poly[, "pop"] <- NULL
  pop.poly[, "gating_id"] <- NULL
  pop.poly[, "point_order"] <- NULL
  poly.log <- as.matrix(pop.poly)

  return(poly.log)
}

#' Get cruise name
#'
#' @param db SQLite3 database file path.
#' @return Cruise name
#' @export
get_cruise <- function(db) {
  meta <- get_metadata_table(db)
  if (nrow(meta) == 0) {
    stop(paste0("No cruise name found, metadata table is empty"))
  }
  return(meta$cruise[1])
}

#' Get instrument serial number
#'
#' @param db SQLite3 database file path.
#' @return One serial number
#' @export
get_inst <- function(db) {
  meta <- get_metadata_table(db)
  if (nrow(meta) == 0) {
    stop(paste0("No instrument serial found, metadata table is empty"))
  }
  return(meta$inst[1])
}

#' Return a tibble of the metadata table of cruise and instrument serial.
#'
#' @param db SQLite3 database file path.
#' @return Tibble for metadata table
#' @export
get_metadata_table <- function(db) {
  meta <- sql_dbGetQuery(db, "SELECT * FROM metadata;")
  meta <- tibble::as_tibble(meta)
  return(meta)
}

#' Return a tibble for the sfl table.
#'
#' @param db SQLite3 database file path.
#' @param outlier_join Left join to outlier table by file ID, adding an outlier
#'   flag column. This function will not perform any filtering by outlier flag.
#' @return Tibble of sfl table.
#' @export
get_sfl_table <- function(db, outlier_join = TRUE) {
  sfl <- sql_dbGetQuery(db, "SELECT * FROM sfl ORDER BY date ASC")
  sfl <- sfl %>% dplyr::mutate(date = lubridate::ymd_hms(date))
  if (outlier_join) {
    outlier <- sql_dbGetQuery(db, "SELECT file, flag FROM outlier ORDER BY file ASC")
    sfl <- dplyr::left_join(sfl, outlier, by = "file")
  }

  sfl <- tibble::as_tibble(sfl)

  return(sfl)
}

#' Return a tibble for the opp table.
#'
#' @param db SQLite3 database file path.
#' @param sfl_join Join to SFL table by file ID, adding a date column and
#'   removing OPP entries with no corresponding SFL entry.
#' @param all_sfl_columns If joining to SFL, include all SFL columns.
#' @param outlier_join Left join to outlier table by file ID, adding an outlier
#'   flag column. This function will not perform any filtering by outlier flag.
#' @param particles_in_all_quantiles Only keep files that have OPP particles in
#'   all quantiles.
#' @return Tibble of opp table.
#' @export
get_opp_table <- function(db, sfl_join = TRUE, all_sfl_columns = FALSE,
                          outlier_join = TRUE, particles_in_all_quantiles = TRUE) {
  if (!sfl_join) {
    opp <- sql_dbGetQuery(db, "SELECT * FROM OPP ORDER BY file ASC")
  } else {
    if (all_sfl_columns) {
      sql <- "
        SELECT
          opp.*, sfl.*
        FROM opp
        INNER JOIN sfl ON sfl.file == opp.file
        ORDER BY sfl.date ASC"
      opp <- sql_dbGetQuery(db, sql)
      opp <- opp[, !duplicated(colnames(opp))]  # remove duplicate file column
      opp <- opp %>% dplyr::relocate(date)  # move date to first column
    } else {
      sql <- "
        SELECT
          sfl.date, opp.*
        FROM opp
        INNER JOIN sfl ON sfl.file == opp.file
        ORDER BY sfl.date ASC"
      opp <- sql_dbGetQuery(db, sql)
    }
    opp <- opp %>% dplyr::mutate(date = lubridate::ymd_hms(date))
  }

  # Standardize on file_id to match parquet files
  opp <- opp %>% dplyr::rename(file_id = file)

  # Convert to tibble
  opp <- tibble::as_tibble(opp)

  if (outlier_join) {
    outlier <- get_outlier_table(db)
    opp <- dplyr::left_join(opp, outlier, by = "file_id")
  }
  # Only keep files that have focused particles in all quantiles
  if (particles_in_all_quantiles) {
    opp <- opp %>%
      dplyr::group_by(file_id) %>%
      dplyr::filter(all(opp_count > 0)) %>%
      dplyr::ungroup()
  }

  return(opp)
}

#' Return a tibble for the vct table.
#'
#' @param db SQLite3 database file path.
#' @param sfl_join Join to SFL table by file ID, adding a date column and
#'   removing VCT entries with no corresponding SFL entry.
#' @param all_sfl_columns If joining to SFL, include all SFL columns.
#' @param outlier_join Left join to outlier table by file ID, adding an outlier
#'   flag column. This function will not perform any filtering by outlier flag.
#' @return Tibble of vct table.
#' @export
get_vct_table <- function(db, sfl_join = TRUE, all_sfl_columns = FALSE,
                          outlier_join = TRUE) {
  if (!sfl_join) {
    vct <- sql_dbGetQuery(db, "SELECT * FROM vct ORDER BY file ASC")
  } else {
    if (all_sfl_columns) {
      sql <- "
        SELECT
          vct.*, sfl.*
        FROM vct
        INNER JOIN sfl ON sfl.file == vct.file
        ORDER BY sfl.date ASC"
      vct <- sql_dbGetQuery(db, sql)
      vct <- vct[, !duplicated(colnames(vct))]  # remove duplicate file column
      vct <- vct %>% dplyr::relocate(date)  # move date to first column
    } else {
      sql <- "
        SELECT
          sfl.date, vct.*
        FROM vct
        INNER JOIN sfl ON sfl.file == vct.file
        ORDER BY sfl.date ASC"
      vct <- sql_dbGetQuery(db, sql)
    }
    vct <- vct %>% dplyr::mutate(date = lubridate::ymd_hms(date))
  }

  # Standardize on file_id to match parquet files
  vct <- vct %>% dplyr::rename(file_id = file)

  # Convert to tibble
  vct <- tibble::as_tibble(vct)

  if (outlier_join) {
    outlier <- get_outlier_table(db)
    vct <- dplyr::left_join(vct, outlier, by = "file_id")
  }

  return(vct)
}

#' Return a tibble for the filter table.
#'
#' @param db SQLite3 database file path.
#' @return Tibble of filter table.
#' @export
get_filter_table <- function(db) {
  sql <- "SELECT * FROM filter ORDER BY date ASC"
  result <- sql_dbGetQuery(db, sql)
  result <- tibble::as_tibble(result)
  return(result)
}

#' Return a tibble for the gating table.
#'
#' @param db SQLite3 database file path.
#' @return Tibble of gating table.
#' @export
get_gating_table <- function(db) {
  sql <- "SELECT * FROM gating ORDER BY date ASC"
  result <- sql_dbGetQuery(db, sql)
  result <- tibble::as_tibble(result)
  return(result)
}

#' Return a tibble for the gating_plan table.
#'
#' @param db SQLite3 database file path.
#' @return Tibble of gating plan table.
#' @export
get_gating_plan_table <- function(db) {
  sql <- "SELECT * FROM gating_plan;"
  result <- sql_dbGetQuery(db, sql)
  result <- tibble::as_tibble(result) %>%
    dplyr::mutate(start_date = lubridate::ymd_hms(start_date))
  return(result)
}

#' Return a tibble for the filter_plan table.
#'
#' @param db SQLite3 database file path.
#' @return Tibble of filter plan table
#' @export
get_filter_plan_table <- function(db) {
  sql <- "SELECT * FROM filter_plan;"
  result <- sql_dbGetQuery(db, sql)
  result <- tibble::as_tibble(result) %>%
    dplyr::mutate(start_date = lubridate::ymd_hms(start_date))
  return(result)
}

#' Return a data frame for the poly table.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of poly table.
#' @export
get_poly_table <- function(db) {
  sql <- "SELECT * FROM poly ORDER BY gating_id, pop, point_order ASC"
  result <- sql_dbGetQuery(db, sql)
  return(result)
}

#' Get tibble of outliers.
#'
#' @param db SQLite3 database file path.
#' @return Tibble of outlier table.
#' @export
get_outlier_table <- function(db) {
  sql <- "SELECT * FROM outlier ORDER BY file;"
  result <- sql_dbGetQuery(db, sql)
  result <- tibble::as_tibble(result) %>%
    dplyr::rename(file_id = file)
  return(result)
}

#' Get aggregate statistics data frame joining sfl, opp, and vct table entries.
#'
#' @param db SQLite3 database file path.
#' @return Data frame of aggregate statistics.
#' @export
get_raw_stat_table <- function(db) {
  if (nrow(get_sfl_table(db)) == 0) {
    stop("SFL table is empty")
  }
  sql <- "SELECT * FROM stat;"
  stat <- sql_dbGetQuery(db, sql)
  stat <- stat %>%
    dplyr::rename(file_id = file)

  return(stat)
}

#' Save VCT aggregate population statistics for one file to vct table.
#'
#' @param db SQLite3 database file path.
#' @param vct_stats DataFrame of VCT statistics created by prep_vct_stats()
#' @return None
#' @export
save_vct_stats <- function(db, vct_stats) {
  # Make sure duplicate entries keyed by file are overwritten with new vct
  # stats. This can happen when using a new gating ID for the same file.
  # Get current VCT table
  old_vct_stats <- get_vct_table(db, outlier_join = FALSE)
  old_vct_stats$date <- NULL  # date is not in VCT table, this is added from SFL
  # Only keep rows in old that don't have a matching file in new
  only_old <- dplyr::anti_join(old_vct_stats, vct_stats, by=c("file_id"))
  # Merge old and new
  merged_vct_stats <- dplyr::bind_rows(only_old, vct_stats)
  # Rename file_id to file to match schema
  merged_vct_stats <- merged_vct_stats %>%
    dplyr::rename(file = file_id)
  # Erase existing table
  reset_vct_table(db)
  # Save table with new results
  sql_dbWriteTable(db, name="vct", value=as.data.frame(merged_vct_stats))
}

#' Save OPP aggregate statistics for one file/quantile combo to opp table.
#'
#' @param db SQLite3 database file path.
#' @param opp_stats Data frame or tibble of OPP statistics that matches the opp
#'   table.
#' @return None
#' @export
save_opp_stats <- function(db, opp_stats) {
  old_opp_stats <- get_opp_table(db, sfl_join = FALSE, outlier_join = FALSE, particles_in_all_quantiles = FALSE)
  old_opp_stats$date <- NULL  # date is not in OPP table, this is added from SFL
  # Only keep rows in old that don't have a matching file in new
  only_old <- dplyr::anti_join(old_opp_stats, opp_stats, by=c("file_id"))
  # Merge old and new
  merged_opp_stats <- dplyr::bind_rows(only_old, opp_stats)
  # Rename file_id to file to match schema
  merged_opp_stats <- merged_opp_stats %>%
    dplyr::rename(file = file_id)
  # Erase existing table
  reset_opp_table(db)
  # Save table with new results
  sql_dbWriteTable(db, name="opp", value = as.data.frame(merged_opp_stats))
}

#' Save Outliers in the database
#'
#' @param db SQLite3 database file path.
#' @param outliers Dataframe with "file_id" and "flag" for outliers. Entries for
#'   files already in the database will be overwritten.
#' @param overwrite Overwrite any existing file entries.
#' @return None
#' @export
save_outliers <- function(db, outliers, overwrite = TRUE) {
  old_outliers <- get_outlier_table(db)
  if (overwrite) {
    # Only keep rows in old that don't have a matching file in new.
    # i.e. overwrite mathching entries in old with entries from new
    only_old <- dplyr::anti_join(old_outliers, outliers, by = "file_id")
    merged_outliers <- dplyr::bind_rows(only_old, outliers) %>% dplyr::arrange(file_id)
  } else {
    # Only include new outliers if they aren't already in existing table
    only_new <- dplyr::anti_join(outliers, old_outliers, by = "file_id")
    merged_outliers <- dplyr::bind_rows(old_outliers, only_new) %>% dplyr::arrange(file_id)
  }
  # Rename file column
  merged_outliers <- merged_outliers %>%
    dplyr::rename(file = file_id)
  # Erase existing table
  reset_outlier_table(db)
  # Save table with new results
  sql_dbWriteTable(db, name="outlier", value = as.data.frame(merged_outliers))
}

#' Save filter parameters to the filter table, appending unless as_is.
#'
#' @param db SQLite3 database file path.
#' @param filter_params Data frame of filtering parameters one row per
#'   quantile. Columns should include:
#'   quantile, beads.fsc.small, beads.D1, beads.D2, width,
#'   notch.small.D1, notch.small.D2, notch.large.D1, notch.large.D2,
#'   offset.small.D1, offset.small.D2, offset.large.D1, offset.large.D2.
#' @param filter_id Optional, supply a filter ID. If not provided a UUID string
#'   will be generated.
#' @param as_is Optional, reset filter table and save filter_params without
#'   modification.
#' @return Database filter ID string.
#' @export
save_filter_params <- function(db, filter_params, filter_id = NULL, as_is = FALSE) {
  if (as_is) {
    reset_filter_table(db)
    df <- filter_params
  } else {
    if (is.null(filter_id)) {
      filter_id <- uuid::UUIDgenerate()  # create ID for new entries
    }
    date.stamp <- to_date_str(lubridate::now("UTC"))
    df <- data.frame()
    for (quantile in filter_params$quantile) {
      p <- filter_params[filter_params$quantile == quantile, ]
      if (nrow(p) > 1) {
        stop("Duplicate quantile rows found in parameters passed to save_filter_params()")
      }
      df <- rbind(df, cbind(id=filter_id, date=date.stamp, quantile=quantile,
                            beads_fsc_small=p$beads.fsc.small,
                            beads_D1=p$beads.D1,
                            beads_D2=p$beads.D2,
                            width=p$width,
                            notch_small_D1=p$notch.small.D1,
                            notch_small_D2=p$notch.small.D2,
                            notch_large_D1=p$notch.large.D1,
                            notch_large_D2=p$notch.large.D2,
                            offset_small_D1=p$offset.small.D1,
                            offset_small_D2=p$offset.small.D2,
                            offset_large_D1=p$offset.large.D1,
                            offset_large_D2=p$offset.large.D2))
    }
  }
  sql_dbWriteTable(db, name="filter", value=df)
  return(filter_id)
}

#' Save filter parameters from a TSV file.
#'
#' @param db SQLite3 database file path.
#' @param filter_plan_tsv Filter plan TSV file.
#' @export
save_filter_params_from_file <- function(db, filter_tsv) {
  filter_df <- readr::read_tsv(filter_tsv) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., "\\.", "_"))
  filter_df$instrument <- NULL
  filter_df$cruise <- NULL
  filter_df$date <- to_date_str(filter_df$date)
  make_popcycle_db(db)
  reset_filter_table(db)
  sql_dbWriteTable(db, name = "filter", value = as.data.frame(filter_df))
}

#' Save gating parameters.
#'
#' This creates a set per population entries in the gating table and saves any
#' manual gating polygon coordinates in the poly table.
#'
#' @param db SQLite3 database file path.
#' @param gates.log Named list of per population classification parameters.
#' @param gating_id Optional, supply a gating ID. If not provided a UUID string
#'   will be generated.
#' @return Database gating ID string.
#' @export
save_gating_params <- function(db, gates.log, gating_id = NULL) {
  if (is.null(gating_id)) {
    gating_id <- uuid::UUIDgenerate()  # create primary ID for new entry
  }
  date.stamp <- to_date_str(lubridate::now("UTC"))
  i <- 1  # track order population classification
  for (popname in names(gates.log)) {
    params <- gates.log[[popname]]
    if (params$method == "manual") {
      df <- data.frame(
        id=gating_id, date=date.stamp, pop_order=i, pop=popname,
        method=params$method,
        channel1=colnames(params$poly)[1],
        channel2=colnames(params$poly)[2],
        gate1=NA,
        gate2=NA,
        position1=NA,
        position2=NA,
        scale=NA,
        minpe=NA
      )
      sql_dbWriteTable(db, name="gating", value=df)
      save_poly(db, params$poly, popname, gating_id)
    } else if (params$method == "auto") {
      df <- data.frame(
        id=gating_id, date=date.stamp, pop_order=i, pop=popname,
        method=params$method,
        channel1=params$x,
        channel2=params$y,
        gate1=params$gates[1],
        gate2=params$gates[2],
        position1=params$position[1],
        position2=params$position[2],
        scale=params$scale,
        minpe=params$min.pe
      )
      sql_dbWriteTable(db, name="gating", value=df)
    } else {
      stop(paste0("unrecognized method ", params$method))
    }
    i <- i + 1
  }
  return(gating_id)
}

#' Save gating parameters from a TSV file.
#'
#' @param db SQLite3 database file path.
#' @param gating_tsv Gating parameters TSV file.
#' @export
save_gating_params_from_file <- function(db, gating_tsv) {
  df <- readr::read_tsv(gating_tsv)
  make_popcycle_db(db)
  reset_gating_table(db)
  sql_dbWriteTable(db, name = "gating", value = as.data.frame(df))
}

#' Save gating polygon coordinates in the poly table.
#'
#' These entries will be linked to an entry in the gating table by gating_id.
#'
#' @param db SQLite3 database file path.
#' @param poly.log Named list of per population gating polygons.
#' @param gating_id Foreign key into gating table.
#' @return None
save_poly <- function(db, poly.log, popname, gating_id) {
  df <- data.frame()
  channels <- c("fsc_small", "fsc_perp", "fsc_big", "pe", "chl_small",
                "chl_big")
  # Fill in population name. This is the first field in the table and sets up
  # the data frame to have the correct number of rows.
  df <- data.frame(pop=rep(popname, nrow(poly.log)))
  for (col in channels) {
    # placeholder NAs for each channel
    # doing this first ensures the channel order matches the poly table
    # definition
    df[, col] <- NA
  }
  for (col in colnames(poly.log)) {
    df[, col] <- poly.log[, col]  # fill in defined channel coords
  }
  df$point_order <- seq(nrow(df))  # order of polygon points for this pop
  df$gating_id <- gating_id  # last field in table

  delete_poly_by_id_pop(db, gating_id, popname)
  sql_dbWriteTable(db, name="poly", value=df)
}

#' Save gating polygon coordinates from a TSV file.
#'
#' @param db SQLite3 database file path.
#' @param poly_tsv Gating polygon coordinates TSV file.
#' @export
save_poly_from_file <- function(db, poly_tsv) {
  df <- readr::read_tsv(poly_tsv)
  make_popcycle_db(db)
  reset_poly_table(db)
  sql_dbWriteTable(db, name = "poly", value = as.data.frame(df))
}

#' Save filter plan to db
#'
#' @param db SQLite3 database file path.
#' @param filter_plan Data Frame with two columns: start_date and filter_id.
#'   start_date should be a UTC POSIXct date object.
#' @return None
#' @export
save_filter_plan <- function(db, filter_plan) {
  if (!("start_date") %in% colnames(filter_plan)) {
    stop("filter_plan must contain 'start_date' column")
  }
  if (!("filter_id") %in% colnames(filter_plan)) {
    stop("filter_plan must contain 'filter_id' column")
  }
  if (nrow(filter_plan) == 0) {
    return()
  }

  # Ensure valid date format
  if (any(is.na(filter_plan$start_date))) {
    stop("NA values not allowed in filter_plan table start_date ", which(is.na(filter_plan$start_date)))
  }
  if ("character" %in% class(filter_plan$start_date[1])) {
    filter_plan_tmp <- filter_plan %>%
      dplyr::mutate(start_date = lubridate::ymd_hms(start_date, quiet = T))
    if (any(is.na(filter_plan_tmp$start_date))) {
      stop("bad date strings found at rows ", which(is.na(filter_plan_tmp$start_date)))
    }
  } else if ("POSIXct" %in% class(filter_plan$start_date[1])) {
    # Date should be stored as a string
    filter_plan <- filter_plan %>%
      dplyr::mutate(start_date = to_date_str(start_date))
  } else {
    stop("expected a date string or date object in date column, found ", class(filter_plan$start_date))
  }
  # Check for filter IDs in filter table
  filter_table <- get_filter_table(db)
  bad_filter_ids <- setdiff(filter_plan$filter_id, filter_table$id)
  if (length(bad_filter_ids) > 0) {
    stop("some filter IDs not found in filter table ", bad_filter_ids)
  }

  reset_filter_plan_table(db)
  sql_dbWriteTable(db, name = "filter_plan", value = as.data.frame(filter_plan))
}

#' Save filter parameters from a TSV file.
#'
#' @param db SQLite3 database file path.
#' @param filter_plan_tsv Filter plan TSV file.
#' @export
save_filter_plan_from_file <- function(db, filter_plan_tsv) {
  df <- readr::read_tsv(filter_plan_tsv)
  df$start_date <- to_date_str(df$start_date)
  make_popcycle_db(db)
  reset_filter_plan_table(db)
  sql_dbWriteTable(db, name = "filter_plan", value = as.data.frame(df))
}

#' Save gating plan to db
#'
#' @param db SQLite3 database file path.
#' @param gating_plan Data Frame with two columns: start_date and gating_id.
#'   start_date should be a UTC POSIXct date object.
#' @return None
#' @export
save_gating_plan <- function(db, gating_plan) {
  if (!("start_date") %in% colnames(gating_plan)) {
    stop("gating_plan must contain 'start_date' column")
  }
  if (!("gating_id") %in% colnames(gating_plan)) {
    stop("gating_plan must contain 'gating_id' column")
  }
  if (nrow(gating_plan) == 0) {
    return()
  }

  # Ensure valid date format
  if (any(is.na(gating_plan$start_date))) {
    stop("NA values not allowed in gating_plan table start_date ", which(is.na(gating_plan$start_date)))
  }
  if ("character" %in% class(gating_plan$start_date[1])) {
    gating_plan_tmp <- gating_plan %>%
      dplyr::mutate(start_date = lubridate::ymd_hms(start_date, quiet = T))
    if (any(is.na(gating_plan_tmp$start_date))) {
      stop("bad date strings found at rows ", which(is.na(gating_plan_tmp$start_date)))
    }
  } else if ("POSIXct" %in% class(gating_plan$start_date[1])) {
    # Date should be stored as a string
    gating_plan <- gating_plan %>%
      dplyr::mutate(start_date = to_date_str(start_date))
  } else {
    stop("expected a date string or date object in date column, found ", class(gating_plan$start_date))
  }
  # Check for gating IDs in gating table
  gating_table <- get_gating_table(db)
  bad_gating_ids <- setdiff(gating_plan$gating_id, gating_table$id)
  if (length(bad_gating_ids) > 0) {
    stop("some gating IDs not found in gating table ", paste(bad_gating_ids, collapse=", "))
  }

  reset_gating_plan_table(db)
  sql_dbWriteTable(db, name = "gating_plan", value = as.data.frame(gating_plan))
}

#' Save gating parameters from a TSV file.
#'
#' @param db SQLite3 database file path.
#' @param gating_plan_tsv Gating plan TSV file.
#' @export
save_gating_plan_from_file <- function(db, gating_plan_tsv) {
  df <- readr::read_tsv(gating_plan_tsv)
  df$start_date <- to_date_str(df$start_date)
  make_popcycle_db(db)
  sql_dbWriteTable(db, name = "gating_plan", value = as.data.frame(df))
}

#' Save metadata to db
#'
#' @param db SQLite3 database file path.
#' @param metadata One row data frame or tibble of cruise name and instrument
#'   serial as "cruise" and "inst".
#' @export
save_metadata <- function(db, metadata) {
  if (nrow(metadata) == 0) {
    warning("metadata data frame is empty")
    return()
  }
  if (nrow(metadata) > 1) {
    stop("metadata data frame has more than one row")
  }

  reset_metadata_table(db)
  sql_dbWriteTable(db, name = "metadata", value = as.data.frame(metadata))
}

#' Save metadata from an SFL file name.
#'
#' @param db SQLite3 database file path.
#' @param sfl_tsv SFL TSV file.
#' @export
save_metadata_from_file <- function(db, sfl_tsv) {
  sfl_tsv <- tools::file_path_sans_ext(basename(sfl_tsv))
  parts <- stringr::str_split_1(sfl_tsv, "_")
  inst <- parts[length(parts)]
  cruise <- stringr::str_sub(sfl_tsv, 1, stringr::str_length(sfl_tsv) - stringr::str_length(inst) - 1)
  make_popcycle_db(db)
  reset_metadata_table(db)
  sql_dbWriteTable(db, name = "metadata", value = data.frame(cruise = cruise, inst = inst))
}

#' Save SFL to db
#'
#' @param db SQLite3 database file path.
#' @param sfl SFL data frame or tibble
#' @export
save_sfl <- function(db, sfl) {
  if (nrow(sfl) == 0) {
    warning("sfl data frame is empty")
    return()
  }

  reset_sfl_table(db)
  sql_dbWriteTable(db, name = "sfl", value = as.data.frame(sfl))
}

#' Save SFL from a TSV file.
#'
#' @param db SQLite3 database file path.
#' @param sfl_tsv SFL TSV file.
#' @export
save_sfl_from_file <- function(db, sfl_tsv) {
  df <- readr::read_tsv(sfl_tsv) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., " ", "_")) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename(ocean_tmp = ocean_temp)

  df$date <- to_date_str(df$date)
  make_popcycle_db(db)
  reset_sfl_table(db)
  sql_dbWriteTable(db, name = "sfl", value = as.data.frame(df))
}

#' Create a new, empty sqlite3 popcycle database.
#'
#' If a database already exists missing tables and indexes will be added.
#'
#' @param db SQLite3 database file path.
#' @return None
#' @export
make_popcycle_db <- function(db) {
  sql.file <- system.file(file.path("sql", "popcycle.sql"), package="popcycle")
  cmd <- sprintf("sqlite3 %s < %s", db, sql.file)
  status <- system(cmd)
  if (status > 0) {
    stop(paste("Db creation command '", cmd, "' failed with exit status ", status))
  }
}

#' Wrapper to run dbGetQuery and clean up connection on error.
#'
#' Use for SELECT statements only, otherwise use sql_dbExecute.
#'
#' @param db SQLite3 database file path.
#' @param sql SQL query to run.
#' @return Data frame returned by dbGetQuery.
sql_dbGetQuery <- function(db, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db)
  tryCatch({
    resp <- DBI::dbGetQuery(con, sql)
    DBI::dbDisconnect(con)
    return(resp)
  }, error=function(e) {
    DBI::dbDisconnect(con)
    stop(e)
  })
}

#' Wrapper to run dbExecute and clean up connection on error.
#'
#' Use for any statement except SELECT, in which case use sql_dbGetQuery.
#'
#' @param db SQLite3 database file path.
#' @param sql SQL statement to run.
#' @return Number of rows affected
sql_dbExecute <- function(db, sql) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db)
  tryCatch({
    resp <- DBI::dbExecute(con, sql)
    DBI::dbDisconnect(con)
    return(resp)
  }, error=function(e) {
    DBI::dbDisconnect(con)
    stop(e)
  })
}

#' Wrapper to run dbWriteTable and clean up connection on error.
#'
#' @param db SQLite3 database file path.
#' @param name Table name.
#' @param value Data frame to write.
sql_dbWriteTable <- function(db, name, value) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=db)
  tryCatch({
    DBI::dbWriteTable(conn=con, name=name, value=value, row.names=F, append=T)
    DBI::dbDisconnect(con)
  }, error=function(e) {
    DBI::dbDisconnect(con)
    stop(e)
  })
}

#' Find common database files between two directories.
#'
#' Directories will be search recursively and files will be matched by
#' basename. An error will be thrown if the same database file occurs more than
#  once in the same top-level directory.
#'
#' @param dir_a, dir_b Directories to compare.
#' @return Data Frame with columns for basename, old_path, and new_path
find_common_dbs <- function(dir_a, dir_b) {
  # First find DB files with the same basename
  dbs_a <- list.files(dir_a, recursive=TRUE, pattern=".*\\.db")
  dbs_b <- list.files(dir_b, recursive=TRUE, pattern=".*\\.db")
  dbs_a_base <- sapply(dbs_a, basename)
  dbs_b_base <- sapply(dbs_b, basename)

  # stop if duplicated DBs, by basename, within either list
  dups_a <- duplicated(dbs_a_base)
  dups_b <- duplicated(dbs_b_base)
  if (any(dups_a)) {
    stop(paste0("Duplicated databases detected in ", dir_a, ": ", unique(dbs_a_base[dups_a])))
  }
  if (any(dups_b)) {
    stop(paste0("Duplicated databases detected in ", dir_b, ": ", unique(dbs_b[dups_b])))
  }

  # Find dbs in common
  common <- intersect(dbs_a_base, dbs_b_base)
  common_a_idx <- match(common, dbs_a_base)
  common_b_idx <- match(common, dbs_b_base)

  # Return db file matches as a dataframe with columns for basename,
  # old file paths, new file paths
  df <- data.frame(
    basename=common,
    old_path=sapply(dbs_a[common_a_idx], function(x) file.path(dir_a, x)),
    new_path=sapply(dbs_b[common_b_idx], function(x) file.path(dir_b, x)),
    stringsAsFactors=FALSE
  )
  row.names(df) <- NULL  # otherwise row names will equal names(common)
  return(df)
}

#' Copy tables from one popcycle database to another.
#'
#' The source database will remained unchanged. The destination database will
#' have selected table cleared before the copy. Schemas for source and
#' destination tables must match or an error is thrown.
#' @param db_from Popcycle database to copy tables from.
#' @param db_to Popcycle database to copy tables to.
#' @param tables Tables to copy.
#' @return None
copy_tables <- function(db_from, db_to, tables) {
  # If dbs are the same file do nothing. This prevents erroneously erasing
  # tables then trying to copy from the just deleted tables.
  db_from <- normalizePath(db_from, mustWork=T)
  db_to <- normalizePath(db_to, mustWork=T)
  if (db_from == db_to) {
    print("Not copying db tables, db files are the same")
    return()
  }

  for (table_name in tables) {
    # Make sure columns match for table to copy
    col_from <- colnames(sql_dbGetQuery(db_from, paste0("SELECT * FROM ", table_name)))
    col_to <- colnames(sql_dbGetQuery(db_to, paste0("SELECT * FROM ", table_name)))
    if (! identical(col_from, col_to)) {
      stop(paste0("db files have differing columns for ", table_name, " table"))
    }

    # Clear the db_to table
    reset_table(db_to, table_name)

    # Get the db_from table
    table_from <- sql_dbGetQuery(db_from, paste0("select * from ", table_name))

    # Save to db_to table
    sql_dbWriteTable(db_to, name=table_name, value=table_from)
  }
}

#' Copy outlier entries from src_db to dest_db.
#'
#' This assumes db_to already has a complete outlier table with entries for
#' every file in the opp table. Entries in db_to with flag values in db_from
#' > 0 will be updated to reflect db_from. db_from entries without a
#' corresponding entry in db_to will be ignored.
#' @param db_from Popcycle database to copy flags > 0 from.
#' @param db_to Popcycle database to copy flags > 0 to.
#' @return None
copy_outlier_table <- function(db_from, db_to) {
  db_from <- normalizePath(db_from, mustWork=T)
  db_to <- normalizePath(db_to, mustWork=T)
  # If dbs are the same file do nothing.
  if (db_from == db_to) {
    print("Not copying outlier tables, db files are the same")
    return()
  }

  src <- get_outlier_table(db_from) %>% dplyr::rename(file = file_id)
  dest <- get_outlier_table(db_to) %>% dplyr::rename(file = file_id)
  joined <- merge(x=src, y=dest, by="file", all.y=TRUE)
  # So we don't screw anything up and because merge may reorder rows by "by"
  # column, enforce a common sort order by "file" on both dataframes we'll use
  # going forward.
  dest <- dest[order(dest$file), ]
  joined <- joined[order(joined$file), ]

  # Any file in dest not in src are set with flag 0
  x_gt_0 <- (joined$flag.x > 0) & !is.na(joined$flag.x)
  joined[x_gt_0, "flag.y"] <- joined[x_gt_0, "flag.x"]
  # Check that file list in joined matches dest
  if (any(joined$file != dest$file)) {
    stop("copy_outlier_table produced an incorrect result")
  }
  dest$flag <- as.integer(joined$flag.y)
  reset_table(db_to, "outlier")
  sql_dbWriteTable(db_to, name="outlier", value=dest)
}

#' Get aggregate statistics data frame along with estimates of cell abundance.
#'
#' @param db SQLite3 database file path.
#' @param inst Instrument serial. If not provided will attempt to read from db.
#' @return Data frame of aggregate statistics.
#' @export
get_stat_table <- function(db, inst=NULL) {
  if (is.null(inst)) {
    inst <- get_inst(db)
  }

  stat <- get_raw_stat_table(db)
  outliers <- get_outlier_table(db)

  #merge stat table with outlier table
  stat <- merge(stat, outliers, all.x=T)

  fr <- flowrate(stat$stream_pressure, inst=inst)

  stat[,"flow_rate"] <- fr[,1]
  stat[,"flow_rate_se"] <- fr[,2]

  # abundance is calculated based on a median value of opp_evt ratio for the
  # entire cruise (volume of virtual core for an entire cruise), except for
  # prochloro which uses the per-file ratio
  qratios <- stat %>%
    dplyr::group_by(time, quantile) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%  # this just gets the single value per file,quantile which is duplicated for each pop
    dplyr::ungroup() %>%
    dplyr::group_by(quantile) %>%  # now we have one ratio per file,quantile. group by quantile to create 3 groups with one ratio per file
    dplyr::summarize(opp_evt_ratio = median(opp_evt_ratio, na.rm=T))  # median of each quantile without double-counting for population duplicates

  for (q in qratios$quantile) {
    ratio <- qratios[qratios$quantile == q, "opp_evt_ratio"][[1]]
    qindex <- stat$quantile == q
    stat[qindex, c("abundance")]  <- stat[qindex, "n_count"] / (1000* ratio * stat[qindex, "flow_rate"] * stat[qindex, "file_duration"]/60)   # cells µL-1
  }
  # Now prochloro
  proindex <- stat$pop == "prochloro"
  stat[proindex, c("abundance")] <- stat[proindex, "n_count"] / (1000 * stat[proindex, "opp_evt_ratio"] * stat[proindex, "flow_rate"] * stat[proindex, "file_duration"] / 60)  # cells µL-1

  # Add abundance SE
  stat$abundance_se <- stat$abundance * stat$flow_rate_se / stat$flow_rate  # cells µL-1

  return(stat)
}

#' Migrate filtering, gating, and outlier related tables to a new post-filter database.
#'
#' Tables migrated are filter, filter_plan, gating_plan, gating, poly, and outlier.
#'
#' @param db_from Database to copy outlier table from.
#' @param db_to Database to copy outlier table to.
#' @export
migrate_tables <- function(old_db, new_db) {
  copy_tables(old_db, new_db, c("filter", "filter_plan", "gating_plan", "gating", "poly"))
  copy_outlier_table(old_db, new_db)
}
