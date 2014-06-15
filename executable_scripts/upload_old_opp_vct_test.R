library(popcycle)

full.path <- 'path/to/KiloMoana_1'
files <- list.files(full.path, recursive=T, full.name=T)
opp.list <- files[!grepl('.opp.', files)]
opp.list <- opp.list[grepl('.opp', opp.list)]
vct.list <- files[grepl('.vct', files)]
cruise.name <- 'KiloMoana_1'
db = 'path/to/popcycle.db'

for (i in 1:length(opp.list)) {
  print(opp.list[i])
  opp <- readSeaflow(opp.list[i])
  #vct <- read.table(paste0(opp.list[i], '.consensus.vct'), header=T)[,1]
  file.split <- strsplit(opp.list[i], '/')[[1]]
  len <- length(file.split)
  file.name = paste(file.split[len - 1], file.split[len], sep='/')
  file.name = gsub('.opp', '', file.name) 
  upload.opp(opp.to.db.opp(opp, cruise.name, file.name), db)
}

for (i in 1:length(vct.list)) {
  print(vct.list[i])
  vct <- read.table(vct.list[i], header=T)[,1]
  file.split <- strsplit(vct.list[i], '/')[[1]]
  len <- length(file.split)
  day = file.split[len - 1]
  file = file.split[len]
  file = strsplit(file, '.opp')[[1]][1]
  file.name = paste(day, file, sep='/')
  file.name = gsub('.opp', '', file.name) 
  upload.opp(opp.to.db.opp(opp, cruise.name, file.name), db)
}

