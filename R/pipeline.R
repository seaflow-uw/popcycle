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
#'              '2014_185/2014-07-04T00-00-02+00-00')
#' }
#' @export
evaluate.evt <- function(db, evt.dir, opp.dir, vct.dir, evt.file) {
  if (length(evt.file) == 0) {
    print('No data collected yet.')
    return()
  }

  print(paste('Analyzing', evt.file))
  print(paste('Filtering', evt.file))
  filter.evt.files(db, evt.dir, evt.file, opp.dir)
  print(paste('Classifying', evt.file))
  classify.opp.files(db, opp.dir, evt.file, vct.dir)
}

#' Merge databases after refiltering and reanalyze OPP.
#'
#' This functions automates many parts of common analysis that occur downstream
#' from raw particle filtration, e.g. gating. It will clear the gating and poly
#' tables in databases in dir_new and copy the same tables from corresponding
#' databases found in dir_old. After the new databases have been updated, OPP
#' particles will be reclassified and diameters and carbon quotas will be
#' recalculated.
#' @param dir_old Directory with old popcycle databases. Database files will be
#'   searched for recursively. Only the database files are used. Other results
#'   files are ignored.
#' @param dir_new Directory with new filtered results. Database files will be
#'   searched for recursively. For each database file it is expected that there
#'   will be corresponding OPP directories in the same location, with a naming
#'   scheme of <cruise>_opp/. <cruise> should match the cruise stored in the
#'   metadata table.
#' @return None
#' @examples
#' \dontrun{
#' merge_and_reanalyze(dir_old, dir_new)
#' }
#' @export
merge_and_reanalyze <- function(dir_old, dir_new) {
  common_dbs <- find_common_dbs(dir_old, dir_new)

  if (nrow(common_dbs)) {
    print(paste0('Found ', nrow(common_dbs), ' common database file(s)'))
    print(common_dbs)

    # Copy tables needed to reanalyze after refiltering
    # Then regate
    for (i in seq(nrow(common_dbs))) {
      common <- common_dbs[i, ]

      print(paste0('Merging ', common$old_path, ' into ', common$new_path))
      copy_tables(common$old_path, common$new_path, c('metadata','gating', 'poly'))
      # Outlier table is a bit trickier and needs its own function
      copy_outlier_table(common$old_path, common$new_path)

      print(paste0('Extracting VCT table from ', common$old_path))
      old_vct_table <- get.vct.table(common$old_path)
      # Sometimes a VCT table has entries for OPP files that are not in the
      # latest set of OPP files for that database for whatever reason. Do a
      # merge here to make sure that only VCT entries for OPP files returned by
      # get.opp.files(old_db, outliers=F) are considered.
      old_opp_files <- get.opp.files(common$old_path, outliers=F)
      # This will remove VCT entries not in old_opp_files
      joined_vct <- merge(x=old_vct_table, y=data.frame(file=old_opp_files), by="file", all.y=T)
      # This will remove opp file entries not in VCT
      joined_vct <- joined_vct[!is.na(joined_vct$gating_id), ]

      print(paste0('Classifying with ', common$new_path))
      working_dir <- dirname(common$new_path)
      cruise <- get.cruise(common$new_path)
      opp_dir <- file.path(working_dir, paste0(cruise, '_opp'))
      vct_dir <- file.path(working_dir, paste0(cruise, '_vct'))
      opp_files <- get.opp.files(common$new_path, outliers=F)
      # Predict diameter, carbon quota, classify
      # diameter call here
      # carbon quota call here
      classify.opp.files(db=common$new_path, opp.dir=opp_dir,  opp.files=opp_files, vct.dir=vct_dir, vct.table=joined_vct)
    }
  }
}
