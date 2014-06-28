if (!('RSQLite' %in% rownames(installed.packages()))) {
  install.packages('RSQLite', repos='http://cran.us.r-project.org')
}

if (!('splancs' %in% rownames(installed.packages()))) {
  install.packages('splancs', repos='http://cran.us.r-project.org')
}

if (!('plyr' %in% rownames(installed.packages()))) {
  install.packages('plyr', repos='http://cran.us.r-project.org')
}

if (!('maps' %in% rownames(installed.packages()))) {
  install.packages('maps', repos='http://cran.us.r-project.org')
}

if (!('mapdata' %in% rownames(installed.packages()))) {
  install.packages('mapdata', repos='http://cran.us.r-project.org')
}

if (!('plotrix' %in% rownames(installed.packages()))) {
  install.packages('plotrix', repos='http://cran.us.r-project.org')
}


install.packages('.', repos=NULL, type='source')
