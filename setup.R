install.packages("devtools", repos='http://cran.us.r-project.org')
library(devtools)
install(dependencies=c("Depends", "Imports", "Suggests"),
        repos='http://cran.us.r-project.org')
