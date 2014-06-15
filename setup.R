if (!('RSQLite' %in% rownames(installed.packages()))) {
  install.packages('RSQLite', repos='http://cran.us.r-project.org')
}

if (!('splancs' %in% rownames(installed.packages()))) {
  install.packages('splancs', repos='http://cran.us.r-project.org')
}

install.packages('.', repos=NULL, type='source')
