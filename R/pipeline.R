#main function
evaluate.evt <- function(db, cruise, evt.dir, opp.dir, vct.dir, evt.file) {
  if (length(evt.file) == 0) {
    print('No data collected yet.')
    return()
  }

  print(paste('Analyzing', evt.file))

  #upload evt count
  file.name = clean.file.path(evt.file)
  print(paste('Filtering', evt.file))
  filter.evt.files(db, cruise, evt.dir, evt.file, opp.dir)

  #classify opp
  print(paste('Classifying', evt.file))
  run.gating(db, cruise, opp.dir, evt.file, vct.dir)
}
