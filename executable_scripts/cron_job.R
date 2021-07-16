#!/usr/bin/env Rscript

parser <- optparse::OptionParser(usage="usage: realtime-classify.R --db FILE --vct-dir FILE --opp-dir DIR [options]")
parser <- optparse::add_option(parser, c("--db"), type="character", default="",
  help="Popcycle database file. Required.",
  metavar="FILE")
parser <- optparse::add_option(parser, c("--opp-dir"), type="character", default="",
  help="OPP directory. Required.",
  metavar="DIR")
parser <- optparse::add_option(parser, c("--vct-dir"), type="character", default="",
  help="VCT directory. Required.",
  metavar="DIR")
parser <- optparse::add_option(parser, c("--stats-file"), type="character", default="",
  help="Stats table output file.",
  metavar="FILE")
parser <- optparse::add_option(parser, c("--sfl-file"), type="character", default="",
  help="SFL table output file.",
  metavar="FILE")
parser <- optparse::add_option(parser, c("--plot-vct-file"), type="character", default="",
  help="VCT plot output file.",
  metavar="FILE")
parser <- optparse::add_option(parser, c("--plot-gates-file"), type="character", default="",
  help="Gates plot output file.",
  metavar="FILE")

p <- optparse::parse_args2(parser)
if (p$options$db == "" || p$options$opp_dir == "" || p$options$vct_dir == "") {
  # Do nothing if db, opp_dir, vct_dir are not specified
  message("error: must specify all of --db, --opp-dir, --vct-dir")
  optparse::print_help(parser)
  quit(save="no", status=10)
} else {
  db <- normalizePath(p$options$db)
  opp_dir <- normalizePath(p$options$opp_dir)
  vct_dir <- normalizePath(p$options$vct_dir)
}

if (p$options$stats_file != "") {
  stats_file <- normalizePath(p$options$stats_file)
}
if (p$options$sfl_file != "") {
  sfl_file <- normalizePath(p$options$sfl_file)
}
if (p$options$plot_vct_file != "") {
  plot_vct_file <- normalizePath(p$options$plot_vct_file)
}
if (p$options$plot_gates_file != "") {
  plot_gates_file <- normalizePath(p$options$plot_gates_file)
}

if (!dir.exists(opp_dir) || !file.exists(db)) {
  message(paste0("vct_dir or db does not exist"))
  quit(save=FALSE, status=11)
}

inst <- popcycle::get.inst(db)
cruise <-popcycle::get.cruise(db)

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

dated_msg("Start")
message("Configuration:")
message("--------------")
message(paste0("db = ", db))
message(paste0("cruise (from db) = ", cruise))
message(paste0("serial (from db) = ", inst))
message(paste0("opp-dir = ", opp_dir))
message(paste0("vct-dir = ", vct_dir))
message(paste0("stats-file = ", stats_file))
message(paste0("sfl-file = ", sfl_file))
message(paste0("plot-vct-file = ", plot_vct_file))
message(paste0("plot-gates-file = ", plot_gates_file))
message("--------------")

############################
### ANALYZE NEW FILE(s) ###
############################
opp_list <- popcycle::get.opp.files(db, all.files=FALSE)
vct_list <- unique(popcycle::get.vct.table(db)$file)
files_to_gate <- setdiff(opp_list, vct_list)
dated_msg(paste0("gating ", length(files_to_gate), " files"))
popcycle::classify.opp.files(db, opp_dir, files_to_gate, vct_dir)

##########################
### Save Stats and SFL ###
##########################
if (stats_file != "") {
  stat <- popcycle::get.stat.table(db)
  statcols <- c(
    'time', 'lat', 'lon', 'temp', 'salinity', 'par',
    'stream_pressure', 'file_duration', 'event_rate', 'opp_evt_ratio',
    'pop', 'n_count', 'chl_med', 'pe_med', 'fsc_med',
    'diam_mid_med', 'Qc_mid_med', 'quantile', 'flag', 'flow_rate'
  )
  stat <- stat[stat$quantile == 50, statcols]
  dated_msg("saving stats file")
  write.csv(stat, stats_file, row.names=FALSE, quote=FALSE)
}
if (sfl_file != "") {
  sfl <- popcycle::get.sfl.table(db)
  dated_msg("saving SFL file")
  write.csv(sfl, sfl_file, row.names=FALSE, quote=FALSE)
}

######################
### PLOT CYTOGRAMS ###
######################
if (plot_vct_file != "" || plot_gates_file != "") {
  opp_list <- popcycle::get.opp.files(db)
  last_file <- tail(opp_list,1)
  vct <- popcycle::get.vct.by.file(db, vct_dir, last_file, col_select=c("fsc_small", "chl_small", "pop_q50", "q50"))
  vct <- vct[vct$q50, ]
  vct$file <- vct$file_id
  vct$pop <- vct$pop_q50

  if (plot_vct_file != "") {
    dated_msg("creating VCT cytogram")
    ggplot2::ggsave(
      plot_vct_file,
      popcycle::plot_vct_cytogram(vct, "fsc_small","chl_small", transform=FALSE),
      width=10, height=6, unit='in', dpi=150
    )
  }

  if (plot_gates_file != "") {
    dated_msg("creating Gate cytogram")
    ggplot2::ggsave(
      plot_gates_file,
      popcycle::plot_cytogram(vct, para.x="fsc_small", para.y="chl_small", bins=200, transform=FALSE),
      width=10, height=6, unit='in', dpi=150
    )
  }
}

dated_msg("Done")
