library(popcycle)

db <- Sys.getenv("DBFILE")
instrument.dir <- Sys.getenv("INSTRUMENTDIR")
evt.dir <- Sys.getenv("RAWDATADIR")
opp.dir <- file.path(Sys.getenv("RESULTSDIR"), "opp")
vct.dir <- file.path(Sys.getenv("RESULTSDIR"), "vct")
stat.file <- file.path(Sys.getenv("RESULTSDIR"), "sync", "stats.csv")
sfl.file <- file.path(Sys.getenv("RESULTSDIR"), "sync", "sfl.csv")
processed.file <- file.path(Sys.getenv("RESULTSDIR"), "already-processed.txt")
plot.vct.file <- file.path(Sys.getenv("RESULTSDIR"), "vct.cytogram.png")
plot.gate.file <- file.path(Sys.getenv("RESULTSDIR"), "gate.cytogram.png")

inst <- Sys.getenv("SERIAL")
cruise <- Sys.getenv("CRUISE")

evt.files <- get.evt.files(evt.dir)
file_transfer(evt.dir, instrument.dir)

filter.params <- get.filter.params.latest(db)

if (nrow(filter.params) == 0) {
  print("no filter parameters defined yet")
  quit("no")
}

############################
### ANALYZE LAST FILE(s) ###
############################
opp.list <- get.opp.files(db, all.files=TRUE)
evt.list <- get.evt.files(evt.dir)
# Filter out files which have already created an OPP entry or for which
# filtering has been attempted but no OPP was produced.
already.opp.idx <- na.omit(match(opp.list, unlist(lapply(evt.list, clean.file.path))))
if (length(already.opp.idx) != 0) {
  evt.list <- evt.list[-already.opp.idx]
}
for (evt.file in evt.list) {
  tryCatch({
    evaluate.evt(db, evt.dir, opp.dir, vct.dir, evt.file)
  }, error = function(e) {
    cat(paste0("Error evaluating file ", evt.file, ": ", e))
  })
}

######################
### PLOT CYTOGRAMS ###
######################
cex=1.4

last.file <- tail[opp.list,1]
opp <- try(get.opp.by.file(opp.dir, last.file, quantile=50, vct.dir=vct.dir))

print("creating vct.cytogram.png")
png(plot.vct.file,width=15,height=9,unit='in',res=100)
par(mfrow=c(1,2),cex=cex)
plot.vct.cytogram(opp, "fsc_small","chl_small")
plot.vct.cytogram(opp, "fsc_small","pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()

gating.log <- get.gating.params.latest(db)$gates.log

print("creating gate.cytogram.png")
png(plot.gate.file,width=15,height=9,unit='in',res=100)
par(mfrow=c(1,2),cex=cex)
plot.gate.cytogram(opp, gating.log, para.x="fsc_small",para.y="chl_small")
plot.gate.cytogram(opp, gating.log, para.x="fsc_small",para.y="pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()


##################
### PLOT STATS ###
##################
stat <- get.stat.table(db)
sfl <- get.sfl.table(db)
print("saving stat.csv")
write.csv(stat, stat.file, row.names=FALSE, quote=FALSE)
print("saving sfl.csv")
write.csv(sfl, sfl.file, row.names=FALSE, quote=FALSE)
print("DONE")
