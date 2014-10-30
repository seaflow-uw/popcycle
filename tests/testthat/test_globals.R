library(popcycle)

context("Project configuration")

test_that("Project folder is set and created correctly", {
  save <- project.location
  newdir <- tempdir()
  projdir <- file.path(newdir, "project")
  set.project.location(projdir)


  # Test value in exported package env
  expect_equal(popcycle::project.location, file.path(newdir, "project"))
  # Test value in internal namespace env
  expect_equal(popcycle:::project.location, file.path(newdir, "project"))
  # Test that directories and database actually exists
  expect_true(file.exists(file.path(newdir, "project")))
  expect_true(file.exists(file.path(newdir, "project", "logs")))
  expect_true(file.exists(file.path(newdir, "project", "params")))
  expect_true(file.exists(file.path(newdir, "project", "sqlite")))
  expect_true(file.exists(file.path(newdir, "project", "sqlite", "popcycle.db")))

  # Reset project location
  set.project.location(save)

  # Erase temp dir
  unlink(projdir, recursive=T)
})

test_that("EVT location is set correctly", {
  save <- evt.location
  set.evt.location("foobar")

  # Test value in exported package env
  expect_equal(popcycle::evt.location, "foobar")
  # Test value in internal namespace env
  expect_equal(popcycle:::evt.location, "foobar")

  # Reset EVT location
  set.evt.location(save)
})

test_that("Instrument location is set correctly", {
  save <- instrument.location
  set.instrument.location("foobar")

  # Test value in exported package env
  expect_equal(popcycle::instrument.location, "foobar")
  # Test value in internal namespace env
  expect_equal(popcycle:::instrument.location, "foobar")

  # Reset instrument location
  set.evt.location(save)
})

test_that("Cruise ID is set correctly", {
  save <- cruise.id
  set.cruise.id("foobar")

  # Test value in exported package env
  expect_equal(popcycle::cruise.id, "foobar")
  # Test value in internal namespace env
  expect_equal(popcycle:::cruise.id, "foobar")

  # Reset instrument location
  set.cruise.id(save)
})
