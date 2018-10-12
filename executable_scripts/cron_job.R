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
file.transfer(evt.dir, instrument.dir)

filter.params <- get.filter.params.latest(db)

if (nrow(filter.params) == 0) {
  print("no filter parameters defined yet")
  quit("no")
}

############################
### ANALYZE LAST FILE(s) ###
############################
# test
read.attempt <- try(read.table(processed.file, header=F, colClasses="character"))
if (inherits(read.attempt, "try-error")) {
  processed.list <- c()
} else {
  processed.list <- read.attempt[,1]
}
opp.list <- unique(get.opp.table(db)$file)
evt.list <- get.evt.files(evt.dir)
# Filter out files which have already created an OPP entry or for which
# filtering has been attempted but no OPP was produced.
already.opp.idx <- na.omit(match(opp.list, unlist(lapply(evt.list, clean.file.path))))
if (length(already.opp.idx) != 0) {
  evt.list <- evt.list[-already.opp.idx]
}
already.processed.idx <- na.omit(match(processed.list, unlist(lapply(evt.list, clean.file.path))))
if (length(already.processed.idx) != 0) {
  evt.list <- evt.list[-already.processed.idx]
}
for (evt.file in evt.list) {
  tryCatch({
    evaluate.evt(db, evt.dir, opp.dir, vct.dir, evt.file)
  }, error = function(e) {
    cat(paste0("Error evaluating file ", evt.file, ": ", e))
  })
}

# Save list of files we have already processed. This is a temporary kludge
# (really!) to prevent attempting to refilter files that produce no OPP.
# Eventually such files will be tracked in the database.
write.table(evt.list, file=processed.file, row.names=F, col.names=F, append=T)


##################
### PLOT STATS ###
# ################
stat <- get.stat.table(db)
sfl <- get.sfl.table(db)
print("saving stat.csv")
write.csv(stat, stat.file, row.names=FALSE, quote=FALSE)
print("saving sfl.csv")
write.csv(sfl, sfl.file, row.names=FALSE, quote=FALSE)
print("DONE")



######################
### PLOT CYTOGRAMS ###
######################
cex=1.4

last.file <- tail(opp.list,1)
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
plot.gating.cytogram(opp, gating.log, para.x="fsc_small",para.y="chl_small")
plot.gating.cytogram(opp, gating.log, para.x="fsc_small",para.y="pe")
mtext(paste(last.file), side=3, line=-3,outer=T,cex=cex)
dev.off()
