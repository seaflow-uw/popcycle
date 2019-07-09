#!/usr/bin/env Rscript
# To install to a non-default library location, set the environment variable
# R_LIBS_USER.

system2("Rscript", c("./install-deps-only.R"))
system2("Rscript", c("./install-package-only.R"))
