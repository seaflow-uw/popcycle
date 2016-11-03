install.packages("devtools", repos='http://cran.us.r-project.org')
library(devtools)
install(dependencies=c("Depends", "Imports", "Suggests"),
        repos='http://cran.us.r-project.org')

# It would be nice to inlucde this BioConductor package dependency
# in DESCRIPTION, but I'm not sure how to get devtools install to
# recognize bioconductor repos.
source("https://bioconductor.org/biocLite.R")
biocLite("flowDensity")

library(popcycle)
#devtools::test()
