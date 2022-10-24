# renv won't identify Bioconductor dependencies in the DESCRIPTION file.
# This script will manually install CRAN and Bioconductor dependencies in the
# DESCRIPTION file to simulate desired behavior of renv::install() followed by
# renv::snapshot(type="explicit"). i.e. renv is initiated only by dependencies
# listed in the DESCRIPTION file.

# This script should be run after
# renv::init(bare=T)

bioc_deps <- c("flowCore", "flowDensity")

# Parse "Imports" in DESCRIPTION, separating Bioconductor and others
desc <- readLines("DESCRIPTION")
importi <- which(desc == "Imports:")
runs <- rle(startsWith(desc[seq(importi+1, length(desc))], "    "))
deps <- desc[seq(importi+1, importi+runs$length[1])]
deps <- trimws(deps)
deps[seq(1, length(deps)-1)] <- substr(deps[seq(1, length(deps)-1)], 1, nchar(deps[seq(1, length(deps)-1)])-1)

# Install "Imports" deps
renv::install(deps[!(deps %in% bioc_deps)])
renv::install(paste("bioc::", bioc_deps, sep=""))

suggestsi <- which(desc == "Suggests:")
runs <- rle(startsWith(desc[seq(suggestsi+1, length(desc))], "    "))
deps <- desc[seq(suggestsi+1, suggestsi+runs$length[1])]
deps <- trimws(deps)
deps[seq(1, length(deps)-1)] <- substr(deps[seq(1, length(deps)-1)], 1, nchar(deps[seq(1, length(deps)-1)])-1)

# Install "Suggests" deps
renv::install(deps)

# Save deps that have been installed with renv::install() to renv.lock
renv::snapshot(type="all")


# To start this renv project over:
# renv::deactivate()
# erase renv.lock
# erase this path renv::paths$library()
# erase the local ./renv folder