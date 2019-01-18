#!/usr/bin/env Rscript
# Create a full test database from the bare database. Should be run from the
# tests/testdata directory of popcycle repo.
# Arg1: working directory for all updated files
library(popcycle)

args = commandArgs(trailingOnly=TRUE)
wd <- args[1]
dir.create(wd, showWarnings=F, recursive=T)
db <- file.path(wd, "testcruise.db")
file.copy("testcruise_bare.db", db, overwrite=T)
evt_dir <- "evt"
opp_dir <- file.path(wd, "opp")
vct_dir <- file.path(wd, "vct")

evt_files <- get.evt.files(evt_dir)
filter.evt.files(db, evt_dir, evt_files, opp_dir)
opp_files <- get.opp.files(db, outliers=F)
classify.opp.files(db, opp_dir, opp_files, vct_dir)
