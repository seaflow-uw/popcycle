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
  usage = "usage: update_plan_tables.R db",
  description = "Update filter_plan and gating_plan tables based on opp and vct results tables"
)
parser <- optparse::add_option(parser, "--renv",
  type = "character", default = "", metavar = "dir",
  help = "Optional renv directory to use. Requires the renv package."
)


p <- optparse::parse_args2(parser)

if (length(p$args) < 1) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- p$args[1]
  if (!file.exists(db)) {
    stop(paste0(db, " does not exist"), call. = FALSE)
  }
}

popcycle::update_plan_table(db, popcycle::get_opp_table(db))
popcycle::update_plan_table(db, popcycle::get_vct_table(db))
