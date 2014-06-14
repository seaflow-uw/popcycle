if (!('RSQLite' %in% rownames(installed.packages()))) {
  install.packages('RSQLite')
}

if (!('splancs' %in% rownames(installed.packages()))) {
  install.packages('splancs')
}

install.packages('.', repos=NULL, type='source')
