#!/usr/bin/env Rscript
# To install to a non-default library location, set the environment variable
# R_LIBS_USER. To update dependencies, set environment variable
# POPCYCLE_UPDATE_DEPS=TRUE or some other string for which as.logical(x) returns
# TRUE.
update_flag <- as.logical(Sys.getenv("POPCYCLE_UPDATE_DEPS"))
if (is.na(update_flag) | is.null(update_flag)) {
  update_flag <- FALSE
}

# Install and attach devtools
if (!requireNamespace("devtools", quietly=TRUE)) {
  install.packages("devtools", repos='http://cran.us.r-project.org')
}

# Install and attach BiocManager
if (!requireNamespace("BiocManager", quietly=TRUE)) {
  install.packages("BiocManager", repos='http://cran.us.r-project.org')
}
if (!requireNamespace("flowDensity", quietly=TRUE)) {
  BiocManager::install("flowDensity", update=update_flag)
}

# Install this package and the packages it imports
devtools::install(dependencies=TRUE, upgrade=update_flag)
