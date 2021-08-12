#!/usr/bin/env Rscript
library(dplyr, warn.conflicts=FALSE)

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

# ---------------------------------------------------------------------------- #

parser <- optparse::OptionParser(usage="usage: psd.R [options] db vct_dir")
parser <- optparse::add_option(parser, c("--bins"), type="integer", default=85,
  help="Number of bins along each dimension of the distribution [default %default]",
  metavar="N")
parser <- optparse::add_option(parser, c("--no-data.table"), action="store_true", default=FALSE,
  help="Don't use data.table for performance-critical aggregation [default %default]")
parser <- optparse::add_option(parser, c("--out"), type="character", default="PSD",
  help="Output file base path [default %default]",
  metavar="FILE_BASE")
parser <- optparse::add_option(parser, c("--quantile"), type="character", default="2.5",
  help="Quantile. Choices are 2.5, 50, 97.5. [default %default]",
  metavar="QUANTILE")
parser <- optparse::add_option(parser, c("--remove-boundary-points"), action="store_true", default=FALSE,
  help="Remove particles at the per-file boundary of fsc_small, chl_small, pe, diam, Qc [default %default]")
parser <- optparse::add_option(parser, c("--verbose"), action="store_true", default=FALSE,
  help="Print extra diagnostic messages [default %default]")

p <- optparse::parse_args2(parser)
if (length(p$args) < 2) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  db <- normalizePath(p$args[1])
  vct_dir <- normalizePath(p$args[2])
  bins <- p$options$bins
  no_data.table <- p$options$no_data.table
  out <- p$options$out
  quantile_ <- p$options$quantile
  remove_boundary_points <- p$options$remove_boundary_points
  verbose <- p$options$verbose
}

dated_msg("Start")
message("Configuration:")
message("--------------")
message(paste0("db = ", db))
message(paste0("vct-dir = ", vct_dir))
message(paste0("bins = ", bins))
message(paste0("no-data.table = ", no_data.table))
message(paste0("out = ", out))
message(paste0("quantile = ", quantile_))
message(paste0("remove-boundary-points = ", remove_boundary_points))
message(paste0("verbose = ", verbose))

message("--------------")
if (!dir.exists(vct_dir) || !file.exists(db)) {
  message(paste0("vct_dir or db does not exist"))
  quit(save=FALSE)
}
# Create output directory tree
dir.create(dirname(out), recursive=T, showWarnings=F)

cruise <- popcycle::get.cruise(db)
vct_files <- list.files(vct_dir, "\\.parquet$", full.names=T)

meta_full <- popcycle::create_meta(db, as.numeric(quantile_))
meta <- meta_full[, c("date", "volume", "opp_evt_ratio", "flag")]

dated_msg("Retrieving refractive index table")
refracs <- popcycle::read_refraction_csv()
refracs <- refracs[refracs$cruise == cruise, ]
dated_msg(paste0(capture.output(refracs), collapse="\n"))
refracs$cruise <- NULL
if (nrow(refracs) == 0) {
  dated_msg(paste0("refractive index table has no entry for ", cruise, ", using mid for Pro/Syn, lwr for Pico/Croco"))
  refracs <- tibble::as_tibble(data.frame(prochloro="mid", synecho="mid", croco="lwr", picoeuk="lwr"))
}
if ("picoeuk" %in% names(refracs)) {
  refracs$unknown <- refracs$picoeuk
  refracs$beads <- refracs$picoeuk
  dated_msg("using picoeuk refractive index for unknown and beads particles")
} else {
  stop("missing picoeuk defintition from refracs")
}
dated_msg(paste0(capture.output(refracs), collapse="\n"))
if (nrow(refracs) > 1) {
  stop(paste0("refractive index table has multiple entries for ", cruise))
}

dated_msg("Retrieving influx calibration table")
calib_all <- popcycle::read_calib_csv()
calib <- calib_all[calib_all$cruise == cruise, ]
if (length(unique(calib$cruise)) == 0) {
  dated_msg(paste0("calibration table has no entry for ", cruise, ", no calibration will be applied"))
  calib <- NULL
}
dated_msg(paste0(capture.output(calib), collapse="\n"))

grid <- popcycle::create_grid(bins, log_base=2, log_answers=FALSE)
grid_df <- tibble::tibble(fsc_small=grid$fsc_small, pe=grid$pe, chl_small=grid$chl_small, Qc=grid$Qc, diam=grid$diam)
arrow::write_parquet(grid_df, paste0(out, ".grid.parquet"))

# ---------------------------------
# Create the full gridded data file
# ---------------------------------
psd <- popcycle::create_PSD(
  vct_files, quantile_, refracs, grid, log_base=NULL,
  remove_boundary_points=remove_boundary_points, use_data.table=!no_data.table,
  verbose=verbose
)
# Add flags
psd <- dplyr::left_join(psd, meta %>% dplyr::select(date, flag), by="date")
invisible(gc())
dated_msg("Full PSD dim = ", stringr::str_flatten(dim(psd), " "), ", MB = ", object.size(psd) / 2**20)
ptm <- proc.time()
arrow::write_parquet(psd, paste0(out, ".full.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote full parquet in ", deltat[["elapsed"]], " seconds")
invisible(gc())

# -----------
# Hourly file
# -----------
psd <- psd %>% dplyr::filter(flag == 0)  # Remove flagged files
# data.table multi-threading temporarily enabled for grouping
orig_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)
hourly <- popcycle::group_psd_by_time(psd, time_expr="1 hours", use_data.table=!no_data.table)
data.table::setDTthreads(orig_threads)
psd <- tibble::as_tibble(psd)
invisible(gc())

# Add volume-normalized abundances to hourly data
meta <- meta %>% dplyr::filter(flag == 0)  # remove flagged files
hourly_volumes <- popcycle::create_volume_table(meta, time_expr="1 hour")
hourly_psd <- popcycle::add_adundance(hourly, hourly_volumes, calib=calib)

dated_msg("Hourly PSD dim = ", stringr::str_flatten(dim(hourly_psd), " "), ", MB = ", object.size(hourly_psd) / 2**20)
ptm <- proc.time()
arrow::write_parquet(hourly_psd, paste0(out, ".hourly.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly parquet in ", deltat[["elapsed"]], " seconds")

ptm <- proc.time()
readr::write_csv(hourly_psd %>% dplyr::mutate(cruise=cruise) %>% dplyr::rename_with(tolower), paste0(out, ".hourly.csv.gz"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly CSV in ", deltat[["elapsed"]], " seconds")

ptm <- proc.time()
arrow::write_parquet(hourly_volumes %>% dplyr::select(date, volume_file, volume_global), paste0(out, ".hourly-volumes.parquet"))
deltat <- proc.time() - ptm
dated_msg("Wrote hourly volume parquet in ", deltat[["elapsed"]], " seconds")

dated_msg("Finished")
