#!/usr/bin/env Rscript
# To install to a non-default library location, set the environment variable
# R_LIBS_USER.

# Install and attach devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools", repos='http://cran.us.r-project.org')
}

# Install and attach BiocManager
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos='http://cran.us.r-project.org')
}
if (!requireNamespace("flowDensity", quietly = TRUE)) {
  BiocManager::install("flowDensity", update=FALSE)
}

# Install this package and the packages it imports
devtools::install(dependencies=TRUE, upgrade=FALSE)
