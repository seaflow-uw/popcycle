# Install and attach devtools
install.packages("devtools", repos='http://cran.us.r-project.org')

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager", repos='http://cran.us.r-project.org')
}
BiocManager::install("flowDensity", version = "3.8")

# Install this package and the packages it imports
devtools::install(dependencies=TRUE)
