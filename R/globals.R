# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

#evt.location <- '~/SeaFlow/datafiles/evt' # during cruise
evt.location <- '~/SeaFlow/datafiles/evt' # in the lab
instrument.location <- '/Volumes/evt'

# name of SQLite database containing OPP and VCT tables
db.name <- '~/popcycle/sqlite/popcycle.db'
opp.table.name <- 'opp'
vct.table.name <- 'vct'
evt.count.table.name <- 'evt_count'
stats.table.name <- 'stats'
cruise.id <- 'july2014'

# location to log filtering and gating parameters
log.location <- '~/popcycle/logs'
log.gate.location <- paste(log.location, 'gates', sep='/')
log.filter.location <- paste(log.location, 'filter', sep='/')

# location of parameters for filter and gating 
param.location <- '~/popcycle/params'
param.gate.location <- paste(param.location, 'gates', sep='/')
param.filter.location <- paste(param.location, 'filter', sep='/')

# flow calibration for SEAFLOW1
#ratio.evt.stream = 0.14756
#x <- 12 #psi
#flow_rate <- (-9*10^-5 * x^4 + 0.0066 * x^3 - 0.173 * x^2 + 2.5013 * x + 2.1059)  *  ratio.evt.stream # where x is STREAM_PRESSURE (psi)
