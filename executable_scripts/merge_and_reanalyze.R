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
usage <- "usage: merge_and_reanalyze.R dirA dirB [mie.csv]

- dirA should contain the popcycle databases to pull tables from.
- dirB should contain popcycle databases to copy tables to. Database files will
  be matched with those in dirA by name. OPP data must be in the same directory
  as the dirB database file in a subdirectory named '<cruise>_opp' where
  <cruise> is pulled from the metadata table. A new directory '<cruise>_vct'
  will be created in the same location.
- mie.csv is an optional Mie Theory lookup table csv that can be supplied to
  replace the one installed with this version of popcycle."

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
  if (length(args) == 3) {
    mie <- popcycle::read_mie_csv(args[3])
    writeLines(paste0("using Mie theory file ", args[3]))
  } else {
    mie <- NULL
  }
}

popcycle::merge_and_reanalyze(dir_from, dir_to, mie=mie)
