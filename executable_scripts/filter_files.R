#!/usr/bin/env Rscript

parser <- optparse::OptionParser(
  usage = "usage: filter_files.R db evt_dir opp_dir",
  description = "Filter all SeaFlow data for one cruise"
)
parser <- optparse::add_option(parser, "--cores",
  type = "integer", default = 1, metavar = "number",
  help = "Cores to use for processing [default %default]"
)
parser <- optparse::add_option(parser, "--renv",
  type = "character", default = "", metavar = "dir",
  help = "Optional renv directory to use. Requires the renv package."
)

p <- optparse::parse_args2(parser)

if (length(p$args) < 3) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- p$args[1]
  evt_dir <- p$args[2]
  opp_dir <- p$args[3]
  if (!file.exists(db)) {
    stop(paste0(db, " does not exist"), call. = FALSE)
  }
  if (!file.exists(evt_dir)) {
    stop(paste0(evt_dir, " does not exist"), call. = FALSE)
  }
  if (p$options$renv != "") {
    proj_dir <- renv::activate(p$options$renv)
    message("activated renv directory ", proj_dir)
  }
}

message("using popcycle version ", packageVersion("popcycle"))

popcycle::filter_evt_files(db, evt_dir, NULL, opp_dir, cores = p$options$cores)
