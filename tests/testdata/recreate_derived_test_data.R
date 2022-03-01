#!/usr/bin/env Rscript
# Create a full test database from the bare database. Should be run from the
# tests/testdata directory of popcycle repo.
# Arg1: working directory for all updated files
library(popcycle)

args = commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("usage: Rscript recreate_derived_test_data.R workingdir", call.=FALSE)
}
wd <- args[1]
dir.create(wd, showWarnings=F, recursive=T)
db <- file.path(wd, "testcruise.db")
file.copy("testcruise_bare.db", db, overwrite=T, copy.mode=F)
evt_dir <- "evt"
opp_dir <- file.path(wd, "opp")
vct_dir <- file.path(wd, "vct")

evt_files <- get_evt_files(evt_dir)
filter_evt_files(db, evt_dir, evt_files, opp_dir)
opp_files <- get_opp_files(db, outliers=F)
classify_opp_files(db, opp_dir, opp_files, vct_dir)
