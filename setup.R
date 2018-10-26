# Install and attach devtools
install.packages("devtools", repos='http://cran.us.r-project.org')
library(devtools)
# Install this package and the packages it imports
devtools::install(dependencies=TRUE)
