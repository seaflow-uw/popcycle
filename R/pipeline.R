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
    print('No data collected yet.')
    return()
  }

  print(paste('Analyzing', evt.file))
  print(paste('Filtering', evt.file))
  filter.evt.files(db, evt.dir, evt.file, opp.dir)
  print(paste('Classifying', evt.file))
  classify.opp.files(db, opp.dir, evt.file, vct.dir)
}
