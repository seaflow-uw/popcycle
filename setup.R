try_to_install <- function(pkg, repos, type) {
  if (pkg %in% rownames(installed.packages())) {
    return
  }
  if (missing(repos)) {
    repos = 'http://cran.us.r-project.org'
  }
  if (missing(type)) {
    type = getOption("pkgType")
  }
  install.packages(pkg, repos=repos, type=type)
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
try_to_install('.', repos=NULL, type='source')
