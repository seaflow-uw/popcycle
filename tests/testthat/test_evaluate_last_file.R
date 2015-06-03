library(popcycle)

context("Realtime last file pipeline")

test_that("Successfully run realtime last file pipeline", {
  save.project <- project.location
  save.evt <- evt.location

  newdir <- tempdir()
  projdir <- file.path(newdir, "project")

  set.project.location(projdir)
  set.evt.location("../../inst/extdata/SeaFlow/datafiles/evt")
  set.cruise.id("test")

  # Move prepared gating params Rdata and filter params files to params dir
  file.copy("../params.RData", param.gate.location, overwrite=T)
  file.copy("../gating-dummy.csv", param.gate.location, overwrite=T)
  file.copy("../filter.csv", param.filter.location, overwrite=T)

  # Load SFL data
  cmd <- paste("python",
               "../../executable_scripts/fix_sfl.py",
               "--sfl",
               file.path(evt.location, "2014_185", "2014-07-04T00-00-00+00-00.sfl"),
               "--db",
               db.name,
               "--cruise",
               cruise.id,
               sep=" ")
  status <- system(cmd)

  expect_equal(status, 0)

  evaluate.evt()

  stats <- get.stat.table()

  print(paste0("stats$pop = c(", paste(stats$pop, collapse=" "), ")"))
  print(paste0("stats$n_count = c(", paste(stats$n_count, collapse=" "), ")"))

  expect_equal(stats$pop, c("beads", "picoeuk", "prochloro", "synecho", "unknown"))
  expect_equal(stats$n_count, c(6, 10, 44, 12, 13))
  
  # Reset locations
  set.project.location(save.project)
  set.evt.location(save.evt)

  # Erase temp dir
  unlink(projdir, recursive=T)
})
