#!/usr/bin/env Rscript
# Script to copy gating, poly, outlier, metadata tables to a new database
# and repeat analysis downstream from filtering.
#
# usage: merge_and_reanalyze.R dirA dirB
#
# dirA should contain the popcycle databases to pull tables from.
# dirB should contain popcycle databases to copy tables to. Database files will
# be matched with those in dirA by name. OPP data must be in the same directory
# as the dirB database file in a subdirectory named "<cruise>_opp" where
# <cruise> is pulled from the metadata table. A new directory "<cruise>_vct"
# will be created in the same location.
library(popcycle)

usage <- "usage: merge_and_reanalyze.R dirA dirB

- dirA should contain the popcycle databases to pull tables from.
- dirB should contain popcycle databases to copy tables to. Database files will
  be matched with those in dirA by name. OPP data must be in the same directory
  as the dirB database file in a subdirectory named '<cruise>_opp' where
  <cruise> is pulled from the metadata table. A new directory '<cruise>_vct'
  will be created in the same location."

args = commandArgs(trailingOnly=TRUE)

if (length(args) < 2) {
  stop(usage, call.=FALSE)
} else {
  if (! is.na(file.info(args[1])$isdir)) {
    dir_from <- args[1]
  } else {
    stop(paste0("Error: argument ", args[1], " is not a directory"), call.=FALSE)
  }
  if (! is.na(file.info(args[2])$isdir)) {
    dir_to <- args[2]
  } else {
    stop(paste0("Error: argument ", args[2], " is not a directory"), call.=FALSE)
  }
}

merge_and_reanalyze(dir_from, dir_to)
