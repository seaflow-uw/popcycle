#!/usr/bin/env Rscript

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
  if (p$options$renv != "") {
    proj_dir <- renv::activate(p$options$renv)
    message("activated renv directory ", proj_dir)
  }
}

discard <- popcycle::update_plan_table(db, popcycle::get_opp_table(db))
discard <- popcycle::update_plan_table(db, popcycle::get_vct_table(db))
