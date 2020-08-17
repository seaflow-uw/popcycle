#' Filter and classify one EVT file.
#'
#' @param db SQLite3 database file path.
#' @param evt.dir EVT file directory.
#' @param opp.dir OPP file output directory.
#' @param vct.dir VCT file output directory.
#' @param evt.file EVT file to filter. Include julian day directory.
#' @return None
#' @examples
#' \dontrun{
#' evaluate.evt(db, evt.dir, opp.dir, vct.dir,
#'              "2014_185/2014-07-04T00-00-02+00-00")
#' }
#' @export
evaluate.evt <- function(db, evt.dir, opp.dir, vct.dir, evt.file) {
  if (length(evt.file) == 0) {
    print("No data collected yet.")
    return()
  }

  print(paste("Analyzing", evt.file))
  print(paste("Filtering", evt.file))
  filter.evt.files(db, evt.dir, evt.file, opp.dir)
  print(paste("Classifying", evt.file))
  classify.opp.files(db, opp.dir, evt.file, vct.dir)
}

#' Reclassify based on iformation in separate database.
#'
#' This functions automates realysis that occurs downstream from raw particle
#' filtration, e.g. gating, diameter, carbon. It will clear the gating, poly,
#' and metadata tables in the target database and copy the same tables from
#' old_db. It will also copy outlier flags from old_db to the target database
# for files that exist in both databases. After the target database has been
#' updated, OPP particles will be reanalyzed.
#' @param old_db Database file from which to pull gating parameters, metadata,
#'   and outlier flags.
#' @param target_dir Directory with target database file and OPP data. There
#'   must be a database file in target_dir with the same name as old_db. There
#'   should also be an OPP directory with name <cruise>_opp where <cruise> is
#'   the cruise name stored in the metadata table of old_db.
#' @param cores Number of cores to use for VCT creation.
#' @param mie Mie theory lookup table CSV file to use in place of installed table.
#' @param backup If backup is TRUE, create a dated backup copy of database file
#'   and VCT directory found in target_dir before making changes.
#' @return None
#' @examples
#' \dontrun{
#' reclassify(old_db, target_dir, mie="mie.csv", cores=4, backup=TRUE)
#' }
#' @export
reclassify <- function(old_db, target_dir, mie=NULL, cores=1, backup=TRUE) {
  # Database file to pull information from
  print(paste0("Input db = ", old_db))

  # Configure target paths
  print(paste0("Target directory = ", target_dir))
  db <- file.path(target_dir, basename(old_db))
  if (!file.exists(db)) {
    stop(paste0("Matching db file doesn't exist in ", target_dir))
  }
  print(paste0("Target db = ", db))
  cruise <- get.cruise(db)
  opp_dir <- file.path(target_dir, paste0(cruise, "_opp"))
  vct_dir <- file.path(target_dir, paste0(cruise, "_vct"))
  if (!dir.exists(opp_dir)) {
    stop(paste0("OPP directory ", opp_dir, " not found"))
  }
  print(paste0("Target OPP directory = ", opp_dir))
  print(paste0("Target VCT directory = ", vct_dir))

  # Get mie table
  mie_table <- popcycle::read_mie_csv(mie)
  if (!is.null(mie)) {
    print(paste0("Mie table = ", mie))
  } else {
    print(paste0("Using installed Mie table"))
  }

  # Backup existing database file
  if (backup) {
    backup_db <- paste0(
      file.path(target_dir, basename(db)),
      ".backup-",
      lubridate::today()
    )
    print(paste0("Creating backup of target db at ", backup_db))
    file.copy(db, backup_db)
  }
  # Erase or backup existing VCT directory
  if (dir.exists(vct_dir)) {
    if (backup) {
      backup_vct_dir <- paste0(
        file.path(target_dir, basename(vct_dir)),
        ".backup-",
        lubridate::today()
      )
      print(paste0("Creating backup of preexisting target VCT directory at ", backup_vct_dir))
      file.rename(vct_dir, backup_vct_dir)
    } else {
      print(paste0("Erasing preexisting target VCT directory at ", vct_dir))
      unlink(vct_dir, recursive=TRUE)
    }
  }

  print(paste0("Getting gating ID date ranges from VCT table in input db ", old_db))
  old_vct_table <- get.vct.table(old_db)
  # Sometimes a VCT table has entries for OPP files that are not in the
  # latest set of OPP files for that database for whatever reason.
  # i.e. Some VCT table entries may have become out of sync with the OPP
  # table and contain references to files only found in previous filtering
  # runs. It's important to remove those VCT entries here before reanalysis
  # because they may refer to outdated gating parameters which should not
  # be used, and these gating parameters may overlap in time with other more
  # recently defined gating parameters. This will break the time-range based
  # gating that will happen in classify.opp.files. Do a merge to make sure
  # that only VCT entries for OPP files returned by
  # get.opp.files(old_db, outliers=F) are considered.
  old_opp_files <- get.opp.files(old_db, outliers=F)
  # This will remove VCT entries not in old_opp_files
  joined_vct <- merge(
    x=old_vct_table, y=data.frame(file=old_opp_files),
    by="file", all.x=F, all.y=T, sort=F
  )
  # This will remove opp file entries not in VCT
  joined_vct <- joined_vct[!is.na(joined_vct$gating_id), ]
  # The merge step may reorder things so make sure everything is still
  # sorted by ascending date
  joined_vct <- joined_vct[order(joined_vct$date), ]

  # Reset the vct table in the current database. This will be recreated during
  # classification.
  print(paste0("Deleting contents of vct table in target database ", db))
  reset.vct.stats.table(db)
  # Copy tables needed to reclassify
  print(paste0("Merging metadata, gating, poly, and outlier tables from input db ", old_db, " into target db ", db))
  copy_tables(old_db, db, c("metadata", "gating", "poly"))
  # Outlier table is a bit trickier because the file set may have changed after
  # new filtering.
  copy_outlier_table(old_db, db)

  print(paste0("Classifying with target db ", db))
  opp_files <- get.opp.files(db, outliers=F)
  # Predict diameter, carbon quota, classify
  classify.opp.files(
    db=db, opp.dir=opp_dir, opp.files=opp_files,
    vct.dir=vct_dir, vct.table=joined_vct, mie.table=mie_table, cores=cores
  )
}
