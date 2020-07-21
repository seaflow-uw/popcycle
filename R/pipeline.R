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

#' Merge databases after refiltering and reanalyze OPP.
#'
#' This functions automates reanalysis that occurs downstream from raw particle
#' filtration, e.g. gating, diameter, carbon. It will clear the gating, poly,
#' and metadata tables in databases in dir_new and copy the same tables from
#' corresponding databases found in dir_old. It will also copy outlier flags
#' from old databases to new for files that exist in both databases. After the
#' new databases have been updated, OPP particles will be reclassified and
#' diameters and carbon quotas will be recalculated.
#' @param dir_old Directory with old popcycle databases. Database files will be
#'   searched for recursively. Only the database files are used. Other results
#'   files are ignored.
#' @param dir_new Directory with new filtered results. Database files will be
#'   searched for recursively. For each database file it is expected that there
#'   will be corresponding OPP directories in the same location, with a naming
#'   scheme of <cruise>_opp/. <cruise> should match the cruise stored in the
#'   metadata table.
#' @param mie_table Mie theory lookup table to use in place of installed table.
#' @return None
#' @examples
#' \dontrun{
#' merge_and_reanalyze(dir_old, dir_new)
#' }
#' @export
merge_and_reanalyze <- function(dir_old, dir_new, mie_table=NULL) {
  common_dbs <- find_common_dbs(dir_old, dir_new)

  if (nrow(common_dbs)) {
    print(paste0("Found ", nrow(common_dbs), " common database file(s)"))
    print(common_dbs)

    # Copy tables needed to reanalyze after refiltering
    # Then regate
    for (i in seq(nrow(common_dbs))) {
      common <- common_dbs[i, ]

      print(paste0("Merging ", common$old_path, " into ", common$new_path))
      copy_tables(common$old_path, common$new_path, c("metadata","gating", "poly"))
      # Outlier table is a bit trickier because the file set may have changed
      # after new filtering.
      copy_outlier_table(common$old_path, common$new_path)

      print(paste0("Extracting VCT table from ", common$old_path))
      old_vct_table <- get.vct.table(common$old_path)
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
      old_opp_files <- get.opp.files(common$old_path, outliers=F)
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

      print(paste0("Classifying with ", common$new_path))
      working_dir <- dirname(common$new_path)
      cruise <- get.cruise(common$new_path)
      opp_dir <- file.path(working_dir, paste0(cruise, "_opp"))
      vct_dir <- file.path(working_dir, paste0(cruise, "_vct"))
      opp_files <- get.opp.files(common$new_path, outliers=F)
      # Predict diameter, carbon quota, classify
      classify.opp.files(
        db=common$new_path, opp.dir=opp_dir,  opp.files=opp_files,
        vct.dir=vct_dir, vct.table=joined_vct, mie.table=mie_table
      )
    }
  }
}
