#!/usr/bin/env Rscript
# To install to a non-default library location, set the environment variable
# R_LIBS_USER. Set Ncpus option to devtools::install with environment
# variable INSTALL_NCPUS. If not set Ncpus will default to 4.

ncpus <- as.numeric(Sys.getenv("INSTALL_NCPUS"))
if (is.na(ncpus)) {
  ncpus <- 4
}

deps_only <- as.logical(Sys.getenv("DEPS_ONLY"))
if (is.na(deps_only)) {
  deps_only <- FALSE
}

print(paste("Ncpus =", ncpus))
print(paste("Only install dependencies =", deps_only))

# Install devtools
if (!requireNamespace("devtools", quietly=TRUE)) {
  install.packages("devtools", repos='http://cran.us.r-project.org')
}

# Install BiocManager
if (!requireNamespace("BiocManager", quietly=TRUE)) {
  install.packages("BiocManager", repos='http://cran.us.r-project.org')
}

if (deps_only) {
  devtools::install_deps(dependencies=TRUE, upgrade="never", Ncpus=ncpus)
} else {
  devtools::install(dependencies=TRUE, upgrade="never", Ncpus=ncpus)
}
