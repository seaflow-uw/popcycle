try_to_install <- function(pkg) {
  if (pkg %in% rownames(installed.packages())) {
    return(invisible())
  }
  install.packages(pkg, repos='http://cran.us.r-project.org')
  if (!(pkg %in% rownames(installed.packages()))) {
    # Quit R with error status
    q(status = 1)
  }
}

try_to_install('RSQLite')
try_to_install('splancs')
try_to_install('plyr')
try_to_install('maps')
try_to_install('mapdata')
try_to_install('plotrix')

try_to_install_local <- function(pkg) {
  if (pkg %in% rownames(installed.packages())) {
    return(invisible())
  }
  install.packages('.', repos=NULL, type='source')
  if (!(pkg %in% rownames(installed.packages()))) {
    # Quit R with error status
    q(status = 1)
  }
}

try_to_install_local('popcycle')
