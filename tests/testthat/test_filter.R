library(popcycle)

context("EVT filtering")

test_that("Best notch is found", {
  save.project <- project.location
  save.evt <- evt.location

  newdir <- tempdir()
  projdir <- file.path(newdir, "project")

  set.project.location(projdir)
  set.evt.location("../../inst/extdata")

  evt.path <- file.path(evt.location, "SeaFlow", "datafiles", "evt",
                        "2014_135", "2014-05-15T17-07-08+0000")
  evt <- readSeaflow(evt.path)
  notch <- best.filter.notch(evt, notch=seq(0.1, 1.4, by=0.1), width=0.2, do.plot=F)

  expect_equal(notch, 1.1)
  
  # Reset locations
  set.project.location(save.project)
  set.evt.location(save.evt)

  # Erase temp dir
  unlink(projdir, recursive=T)
})

test_that("Successfully filter two files, one core", {
  save.project <- project.location
  save.evt <- evt.location

  newdir <- tempdir()
  projdir <- file.path(newdir, "project")

  set.project.location(projdir)
  set.evt.location("../../inst/extdata")

  evt.path <- c(file.path("SeaFlow", "datafiles", "evt",
                          "2014_135", "2014-05-15T17-16-09+0000"),
                file.path("SeaFlow", "datafiles", "evt",
                          "2014_135", "2014-05-15T17-07-08+0000"))
  filter.evt.files.parallel(c(evt.path), notch=0.8, width=0.2)
  opp.count <- nrow(get.opp.by.file(evt.path[1]))
  opp.count <- opp.count + nrow(get.opp.by.file(evt.path[2]))

  expect_equal(opp.count, 39)
  
  # Reset locations
  set.project.location(save.project)
  set.evt.location(save.evt)

  # Erase temp dir
  unlink(projdir, recursive=T)
})

test_that("Successfully filter two files, two cores", {
  save.project <- project.location
  save.evt <- evt.location

  newdir <- tempdir()
  projdir <- file.path(newdir, "project")

  set.project.location(projdir)
  set.evt.location("../../inst/extdata")

  evt.path <- c(file.path("SeaFlow", "datafiles", "evt",
                          "2014_135", "2014-05-15T17-16-09+0000"),
                file.path("SeaFlow", "datafiles", "evt",
                          "2014_135", "2014-05-15T17-07-08+0000"))
  filter.evt.files.parallel(evt.path, notch=0.8, width=0.2, cores=2)
  opp.count <- nrow(get.opp.by.file(evt.path[1]))
  opp.count <- opp.count + nrow(get.opp.by.file(evt.path[2]))
  
  expect_equal(opp.count, 39)
  
  # Reset locations
  set.project.location(save.project)
  set.evt.location(save.evt)

  # Erase temp dir
  unlink(projdir, recursive=T)
})


test_that("Successfully run realtime pipeline", {
  save.project <- project.location
  save.evt <- evt.location

  newdir <- tempdir()
  projdir <- file.path(newdir, "project")

  set.project.location(projdir)
  set.evt.location("../../inst/extdata")

  evaluate.last.evt()
  opp.count <- nrow(get.opp.by.file(basename(get.latest.evt.with.day())))
  print(opp.count)
    
  # Reset locations
  set.project.location(save.project)
  set.evt.location(save.evt)

  # Erase temp dir
  unlink(projdir, recursive=T)
})
