#!/usr/bin/env Rscript

# optparse may not be installed globally so look for renv directory before
# parsing cli args with optparse
args <- commandArgs(trailingOnly=TRUE)
renv_loc <- args == "--renv"
if (any(renv_loc)) {
  renv_idx <- which(renv_loc)
  if (length(args) > renv_idx) {
    proj_dir <- renv::activate(args[renv_idx + 1])
    message("activated renv directory ", proj_dir)
  }
}

parser <- optparse::OptionParser(
  usage = "usage: filter_files.R db evt_dir opp_dir",
  description = "Filter all SeaFlow data for one cruise"
)
parser <- optparse::add_option(parser, "--cores",
  type = "integer", default = 1, metavar = "number",
  help = "Cores to use for processing [default %default]"
)
parser <- optparse::add_option(parser, "--max-particles-per-file",
  type = "integer", metavar = "number",
  default = popcycle:::MAX_PARTICLES_PER_FILE_DEFAULT,
  help = "Only filter files with an event count <= this limit. Value < 0 disables this limit. [default %default]"
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

  if (p$options$max_particles_per_file < 0) {
    p$options$max_particles_per_file <- NULL
  }
}

message("using popcycle version ", packageVersion("popcycle"))

popcycle::filter_evt_files(
  db, evt_dir, NULL, opp_dir, max_particles_per_file = p$options$max_particles_per_file,
  cores = p$options$cores
)
