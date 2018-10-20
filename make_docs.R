# Scan function documentation in files in R/.
# - Output updated Rd files to man/
# - Update NAMESPACE files
library(devtools)
unlink("./man", recursive=TRUE)
unlink("./NAMESPACE")
devtools::document()
