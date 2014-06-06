# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

evt.location <- '~/SeaFlow/datafiles/evt/'

# name of SQLite database containing OPP and VCT tables
db.name <- '~/popcycle/sqlite/popcycle.db'
opp.table.name <- 'opp'
vct.table.name <- 'vct'
cruise.id <- 'july2014'

# location to log filtering and gating parameters
log.location <- '~/popcycle/logs/'
log.gate.location <- paste(log.location, 'gates/', sep='/')
log.filter.location <- paste(log.location, 'filter/', sep='/')

# location of parameters for filter and gating 
param.location <- '~/popcycle/params/'
param.gate.location <- paste(param.location, 'gates/', sep='/')
param.filter.location <- paste(param.location, 'filter/', sep='/')
