library(magrittr)

#' Reduced gridded data to a subset of the original
#'
#' @param in_prefix Input file prefix, should be output prefix from `grid.R` CLI script.
#' @param out_prefix Output file prefix.
#' @param dim Dimension to reduce down to, e.g. Qc
#' @param pop Population to reduce down to, e.g. prochloro.
#' @param bin_lwr Lower bin to reduce down to.
#' @param bin_upr Upper bin to reduce down to.
reduce_gridded <- function(in_prefix, out_prefix, dim, pop = NULL, bin_lwr = NULL, bin_upr = NULL) {
  # Construct input paths
  hourly_gridded_in_path <- paste0(in_prefix, ".hourly_gridded.parquet")
  grid_bins_in_path <- paste0(in_prefix, ".grid_bins.parquet")
  grid_bins_labels_in_path <- paste0(in_prefix, ".grid_bins_labels.parquet")
  hourly_par_in_path <- paste0(in_prefix, ".hourly_par.parquet")
  hourly_vol_in_path <- paste0(in_prefix, ".hourly_volume.parquet")

  # Construct output paths
  hourly_gridded_out_path <- paste0(out_prefix, ".hourly_gridded.parquet")
  grid_bins_out_path <- paste0(out_prefix, ".grid_bins.parquet")
  grid_bins_labels_out_path <- paste0(out_prefix, ".grid_bins_labels.parquet")
  hourly_par_out_path <- paste0(out_prefix, ".hourly_par.parquet")
  hourly_vol_out_path <- paste0(out_prefix, ".hourly_volume.parquet")

  gridded <- arrow::read_parquet(hourly_gridded_in_path)
  par <- arrow::read_parquet(hourly_par_in_path)
  grid_bins <- arrow::read_parquet(grid_bins_in_path)
  grid_bins_labels <- arrow::read_parquet(grid_bins_labels_in_path)
  volumes <- arrow::read_parquet(hourly_vol_in_path)

  if (length(unique(grid_bins$cruise)) > 1) {
    stop("this tool only supports data with a single cruise")
  }

  # Filter to one population
  if (!is.null(pop)) {
    gridded <- gridded[gridded$pop == pop, ]
  }

  # Group by dim (dim_coord), e.g. Qc (Qc_coord)
  dim_coord <- paste0(dim, "_coord")
  gridded <- gridded %>%
    dplyr::group_by(cruise, date, across(all_of(dim_coord)), pop) %>%
    dplyr::summarise(across(c(n, Qc_sum, n_per_uL, Qc_sum_per_uL), sum), .groups = "drop")

  # Subset grid
  if (is.null(bin_lwr)) {
    bin_lwr <- 0
  }
  if (is.null(bin_upr)) {
    bin_upr <- nrow(grid_bins) - 1
  }

  # Add one to bin_upr to capture the exclusive right edge of the last bin
  grid_bins_labels <- popcycle::grid_bins_labels(as.list(grid_bins %>% select(-c(cruise))))
  grid_bins <- grid_bins[bin_lwr:(bin_upr+1), c("cruise", dim)]
  grid_bins_labels <- tibble::as_tibble(grid_bins_labels)
  grid_bins_labels <- grid_bins_labels %>% dplyr::mutate(cruise = as.factor(grid_bins[[1, "cruise"]]), .before = 1)
  grid_bins_labels <- grid_bins_labels[bin_lwr:bin_upr, c("cruise", dim)]

  print(paste0("grid bins for ", dim,
               " from ", round(grid_bins[[1, dim]], 6),
               " to ", round(grid_bins[[nrow(grid_bins), dim]], 6)))
  print(paste0("bin labels are ",
               grid_bins_labels[[1, dim]],
               " - ",
               grid_bins_labels[[nrow(grid_bins_labels), dim]]))
  # Reduce gridded to match grid subset
  gridded <- gridded[(gridded[[dim_coord]] >= bin_lwr) & (gridded[[dim_coord]] <= bin_upr), ]
  # Reset lower grid index to 1
  gridded[[dim_coord]] <- as.integer(gridded[[dim_coord]] - bin_lwr + 1)

  # Reduce PAR and volumes down to just the dates left in psd
  par <- par[par$date %in% unique(gridded$date), ]
  volumes <- volumes[volumes$date %in% unique(gridded$date), ]

  # Create output directory
  dir.create(dirname(out_prefix), recursive = TRUE, showWarnings = FALSE)

  # Write reduced output
  arrow::write_parquet(gridded, hourly_gridded_out_path)
  arrow::write_parquet(grid_bins, grid_bins_out_path)
  arrow::write_parquet(grid_bins_labels, grid_bins_labels_out_path)
  arrow::write_parquet(par, hourly_par_out_path)
  arrow::write_parquet(volumes, hourly_vol_out_path)
}

t0 <- proc.time()

parser <- optparse::OptionParser(
  usage = "usage: reduce-grid.R [options] in-prefix out-prefix",
  description = "Create gridded distribution data from VCT data"
)
parser <- optparse::add_option(parser, "--dimension",
  type = "character", default = "Qc",
  help = "Dimension to reduce down to. [default %default]"
)
parser <- optparse::add_option(parser, "--bin-lwr",
  type = "integer",
  help = "Optional lower bin index (1-based) to reduce down to."
)
parser <- optparse::add_option(parser, "--bin-upr",
  type = "integer",
  help = "Optional upper bin index (1-based) to reduce down to."
)
parser <- optparse::add_option(parser, "--pop",
  type = "character", default = "",
  help = "Optional population to reduce down to filter."
)

p <- optparse::parse_args2(parser)
if (length(p$args) < 2) {
  optparse::print_help(parser)
  quit(save="no")
} else {
  in_prefix <- p$args[1]
  out_prefix <- p$args[2]
  dimension <- p$options$dimension
  bin_lwr <- p$options$bin_lwr
  bin_upr <- p$options$bin_upr
  pop <- p$options$pop
}

message("using popcycle version ", packageVersion("popcycle"))

library(dplyr, warn.conflicts=FALSE)

dated_msg <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ": ", ...)
}

# ----------
# Begin main
# ----------
dated_msg("Start")

message("Configuration:")
message("--------------")
message("in-prefix = ", in_prefix)
message("out-prefix = ", out_prefix)
message("dimension = ", dimension)
message("bin-lwr = ", bin_lwr)
message("bin-upr = ", bin_upr)
message("pop = ", pop)

reduced <- reduce_gridded(in_prefix, out_prefix, dimension, pop = pop,
                          bin_lwr = bin_lwr, bin_upr = bin_upr)

deltat <- proc.time() - t0
dated_msg("Script ran in ", lubridate::duration(deltat[["elapsed"]]))
