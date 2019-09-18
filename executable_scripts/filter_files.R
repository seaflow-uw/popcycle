#!/usr/bin/env Rscript
library(popcycle)

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 3) {
  stop("filter.R db evt_dir opp_dir", call.=FALSE)
}

db <- args[1]
evt.dir <- args[2]
opp.dir <- args[3]

get.filter.params.latest(db)
evt_files <- get.evt.files(evt.dir)
# Filter files
filter.evt.files(db, evt.dir, evt_files[1:length(evt_files)-1], opp.dir)
