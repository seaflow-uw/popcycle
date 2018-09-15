library(popcycle)

db <- Sys.getenv("DBFILE")
instrument.dir <- Sys.getenv("INSTRUMENTDIR")
evt.dir <- Sys.getenv("RAWDATADIR")
opp.dir <- file.path(Sys.getenv("RESULTSDIR"), "opp")
vct.dir <- file.path(Sys.getenv("RESULTSDIR"), "vct")
stat.file <- file.path(Sys.getenv("RESULTSDIR"), "sync", "stats.csv")
sfl.file <- file.path(Sys.getenv("RESULTSDIR"), "sync", "sfl.csv")
inst <- Sys.getenv("SERIAL")
cruise <- Sys.getenv("CRUISE")

evt.files <- get.evt.files(evt.dir)
file.transfer(evt.dir, instrument.dir)

filter.params <- get.filter.params.latest(db)

if (nrow(filter.params) == 0) {
  print("no filter parameters defined yet")
  quit("no")
}

############################
### ANALYZE LAST FILE(s) ###
############################
opp.list <- unique(get.opp.table(db)$file)
evt.list <- get.evt.files(evt.dir)
id <- match(opp.list, unlist(lapply(evt.list, clean.file.path)))

if (length(id) == 0) {
  for (evt.file in evt.list) {
    evaluate.evt(db, evt.dir, opp.dir, vct.dir, evt.file)
  }
} else {
  for (evt.file in evt.list[-id]) {
    evaluate.evt(db, evt.dir, opp.dir, vct.dir, evt.file)
  }
}

######################
### PLOT CYTOGRAMS ###
######################
# cex=1.4
#
# last.file <- get.latest.evt()
# opp <- get.opp.by.file(last.file)
# vct <- get.vct.by.file(last.file)
# opp$pop <- vct
#
# print("creating vct.cytogram.png")
# png("~/vct.cytogram.png",width=15,height=9,unit='in',res=100)
# par(mfrow=c(1,2),cex=cex)
# plot.vct.cytogram(opp, "fsc_small","chl_small")
# plot.vct.cytogram(opp, "fsc_small","pe")
# mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
# dev.off()
#
# print("creating gate.cytogram.png")
# png("~/gate.cytogram.png",width=15,height=9,unit='in',res=100)
# par(mfrow=c(1,2),cex=cex)
# plot.gate.cytogram(opp, "fsc_small","chl_small")
# plot.gate.cytogram(opp, "fsc_small","pe")
# mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
# dev.off()
#
#
# ##################
# ### PLOT STATS ###
# ##################
stat <- get.stat.table(db)
sfl <- get.sfl.table(db)
print("saving stat.csv")
write.csv(stat, stat.file, row.names=FALSE, quote=FALSE)
print("saving sfl.csv")
write.csv(sfl, sfl.file, row.names=FALSE, quote=FALSE)
print("DONE")
