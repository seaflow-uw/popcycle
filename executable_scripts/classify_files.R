#!/usr/bin/env Rscript

parser <- optparse::OptionParser(
  usage = "usage: classify_files.R db opp_dir vct_dir",
  description = "Classify all SeaFlow data for one cruise"
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
  opp_dir <- p$args[2]
  vct_dir <- p$args[3]
  if (!file.exists(db)) {
    stop(paste0(db, " does not exist"), call. = FALSE)
  }
  if (!file.exists(opp_dir)) {
    stop(paste0(opp_dir, " does not exist"), call. = FALSE)
  }
  if (p$options$renv != "") {
    proj_dir <- renv::activate(p$options$renv)
    message("activated renv directory ", proj_dir)
  }
}

message("using popcycle version ", packageVersion("popcycle"))

popcycle::classify_opp_files(db, opp_dir, NULL, vct_dir, cores = p$options$cores)
