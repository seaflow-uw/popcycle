library(popcycle)
setwd("/Volumes/Samsung_T1")
cruise <- "SCOPE_1"
db <- paste0(cruise, ".db")
make.popcycle.db(db)
evt.dir <- cruise
opp.dir <- paste0(cruise, "_opp")
vct.dir <- paste0(cruise, "_vct")
evt.files <- get.evt.files(evt.dir)


#save.filter.params(db, list(notch1=NA, notch2=NA, offset=0.0, origin=NA, width=0.5))
params <- get.filter.params.latest(db)
params

filter.evt.files(db, cruise, evt.dir, evt.files[1:2], opp.dir)
filter.evt.files(db, cruise, evt.dir, evt.files[2], opp.dir, filter.id=)
load("/Volumes/Samsung_T1/SCOPE_1_realtime_params.Rdata")
poly.log.orig <- poly.log
save.gating.params(db, poly.log)
gating.params <- get.gating.params.latest(db)
run.gating(db, cruise, opp.dir, evt.files[1:10], vct.dir)
run.gating(db, cruise, opp.dir, evt.files[1:10], vct.dir, gating.id="")
evt <- readSeaflow(evt.files[1], evt.dir, transform=FALSE)

filt <- filter.notch(evt, origin=params$origin, width=params$width,
                     notch=c(params$notch1, params$notch2), offset=params$offset)
summary(filt$opp)
filt$params
delete.opp.stats.by.file(db, evt.files[1])
get.opp.stats.by.file(db, evt.files[1])
save.opp.stats(db, "SCOPE_2", evt.files[1], nrow(evt), filt$opp, filt$params, params$uuid)
get.opp.stats.by.file(db, evt.files[1])
save.opp.file(filt$opp, opp.dir, evt.files[1])

opp <- get.opp.by.date(db, opp.dir, "2014-12-09 00:00", "2014-12-09 00:10", vct.dir=vct.dir)
load("/Volumes/Samsung_T1/SCOPE_1_realtime_params.Rdata")
poly.log.orig <- poly.log
poly.log <- set.gating.params(opp, "beads", "fsc_small", "pe")
save.gating.params(db, poly.log)

# Example
library(popcycle)

setwd("/Volumes/Samsung_T1/testcruise/")

cruise <- "testcruise"
evt.dir <- system.file("extdata/SeaFlow/datafiles/evt", package="popcycle")
opp.dir <- "testcruise_opp"
vct.dir <- "testcruise_vct"
db <- "testcruise.db"

make.popcycle.db(db)

file.path(evt.dir, "2014_185", "2014-07-04T00-00-00+00-00.sfl")

# Import SFL
# ...
get.sfl.table(db)

save.filter.params(db)
get.filter.params.latest(db)

evt.files <- get.evt.files(evt.dir)
evt <- readSeaflow(file.path(evt.dir, evt.files[1]), transform=F)
filter.evt.files(db, cruise, evt.dir, evt.files, opp.dir)
opp.files <- get.opp.files(db)
opp <- get.opp.by.file(opp.dir, opp.files)
poly.log <- set.gating.params(opp, "beads", "fsc_small", "pe")
poly.log
poly.log <- set.gating.params(opp, "synecho", "fsc_small", "pe", poly.log)
poly.log <- set.gating.params(opp, "prochloro", "fsc_small", "chl_small", poly.log)
poly.log <- set.gating.params(opp, "picoeuks", "fsc_small", "chl_small", poly.log)
plot.gating.cytogram(opp, poly.log, "fsc_small", "pe")
plot.gating.cytogram(opp, poly.log, "fsc_small", "chl_small")
poly.log

save.gating.params(db, poly.log)
gating.params <- get.gating.params.latest(db)
gating.params$row
gating.params$poly.log

run.gating(db, cruise, opp.dir, opp.files, vct.dir)
get.stat.table(db)
