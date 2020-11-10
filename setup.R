#!/usr/bin/env Rscript
# To install to a non-default library location, set the environment variable
# R_LIBS_USER. Set Ncpus option to devtools::install with environment
# variable INSTALL_NCPUS. If not set Ncpus will default to 4.

ncpus <- as.numeric(Sys.getenv("INSTALL_NCPUS"))
if (is.na(ncpus)) {
  ncpus <- 4
}

cat("Installing using Ncpus =", ncpus, "\n")

# Install and attach devtools
if (!requireNamespace("devtools", quietly=TRUE)) {
  install.packages("devtools", repos='http://cran.us.r-project.org')
}

# Install and attach BiocManager
if (!requireNamespace("BiocManager", quietly=TRUE)) {
  install.packages("BiocManager", repos='http://cran.us.r-project.org')
}

# Prevent warnings from package installs from turning into errors that halt
# installation. This variable is set here because a a minor markup error in a
# flowDensity Rd file prevented it from installing successfully with
# devtools::install_deps. The warning was
# bad markup (extra space?) at plotDens.Rd:62:19
# flowDensity tarball was
# https://bioconductor.org/packages/3.11/bioc/src/contrib/flowDensity_1.22.0.tar.gz
# Web resources that led to this solution
# https://github.com/r-lib/remotes/issues/403
# https://github.com/r-lib/remotes#environment-variables
if (Sys.getenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS") == "") {
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
}
# Install this package and the packages it imports
devtools::install(dependencies=TRUE, upgrade="never", Ncpus=ncpus)
