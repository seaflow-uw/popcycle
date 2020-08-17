#!/usr/bin/env Rscript
description <- "
- source_db should contain gating, poly, outlier, and metadata tables to pull from.

- target_dir is the working directory for reclassification. It should contain a
database that matches source_db's name and an OPP directory named <cruise>_opp.
If there is a VCT directory <cruise>_vct it will be erased and recreated. Gating,
poly, outlier, and metadatea tables in the target db will be overwritten by
information from source_db. The vct table will be erased and recreated during
classification."
parser <- optparse::OptionParser(
  usage="usage: reclassify.R [options] source_db target_dir",
  description=description
)
parser <- optparse::add_option(parser, c("--cores"), type="integer", default=1,
  help="Number of cores to use [default %default]",
  metavar="number")
parser <- optparse::add_option(parser, c("--no-backup"), action="store_true", default=FALSE,
  help="Don't create backups of database and VCT")
parser <- optparse::add_option(parser, c("--mie"), type="character", default=NULL,
  help="Alternate Mie theory lookup table CSV file to use",
  metavar="csv")
p <- optparse::parse_args2(parser)
if (length(p$args) < 2) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  source_db <- p$args[1]
  target_dir <- p$args[2]
  if (!file.exists(source_db)) {
    stop(paste0(source_db, " does not exist"), call.=FALSE)
  }
  if (!dir.exists(target_dir)) {
    stop(paste0(target_dir, " does not exist"), call.=FALSE)
  }
}

popcycle::reclassify(
  source_db, target_dir, cores=p$options$cores, mie=p$options$mie, 
  backup=!p$options$no_backup
)
