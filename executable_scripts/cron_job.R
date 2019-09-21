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

##################
### PLOT STATS ###
##################
stat <- get.stat.table(db)
statcols <- c(
  'time', 'lat', 'lon', 'temp', 'salinity', 'par',
  'stream_pressure', 'file_duration', 'event_rate', 'opp_evt_ratio',
  'pop', 'n_count', 'chl_med', 'pe_med', 'fsc_med',
  'diam_mid_med', 'Qc_mid_med', 'quantile', 'flag', 'flow_rate', 'abundance'
)
stat <- stat[stat$quantile == 50, statcols]
sfl <- get.sfl.table(db)
print("saving stat.csv")
write.csv(stat, stat.file, row.names=FALSE, quote=FALSE)
print("saving sfl.csv")
write.csv(sfl, sfl.file, row.names=FALSE, quote=FALSE)

######################
### PLOT CYTOGRAMS ###
######################
library(tidyverse)
opp.list <- get.opp.files(db, all.files=TRUE)
last.file <- tail(opp.list,1)
opp <- try(get.opp.by.file(opp.dir, last.file, quantile=50, vct.dir=vct.dir))
opp$file <- last.file

print("creating vct.cytogram.png")
plot_vct_cytogram(opp, "fsc_small","chl_small", transform=TRUE)
ggsave(plot.vct.file, width=10, height=6, unit='in', dpi=150)

print("creating gate.cytogram.png")
plot_cytogram(opp, para.x="fsc_small",para.y="chl_small", bins=200, transform=TRUE)
ggsave(plot.gate.file, width=10, height=6, unit='in', dpi=150)

print("DONE")
