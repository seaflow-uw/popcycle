library(popcycle)

context("DB operations")

test_that("Delete SFL rows", {
  newdir <- tempdir()
  projdir <- file.path(newdir, "project")

  set.project.location(projdir)
  set.evt.location("../../inst/extdata/SeaFlow/datafiles/evt")
  set.cruise.id("test")

  # Load SFL data
  cmd <- paste("python",
               "../../executable_scripts/import_sfl.py",
               "--sfl",
               file.path(evt.location, "2014_185", "2014-07-04T00-00-00+00-00.sfl"),
               "--db",
               db.name,
               "--cruise",
               cruise.id,
               sep=" ")
  status <- system(cmd)
  expect_equal(status, 0)
  sfl <- get.sfl.table()
  expect_true(nrow(sfl) > 0)
  popcycle:::.delete.sfl()
  sfl <- get.sfl.table()
  expect_true(nrow(sfl) == 0)

  # Erase temp dir
  unlink(projdir, recursive=T)
})
