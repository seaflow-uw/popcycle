

##########################
### MODIFY ACCORDINGLY ###
##########################

evt.location <- '~/SeaFlow/datafiles/evt' # Path to the evt files for the rel-time analysis
instrument.location <- '/Volumes/evt' # Path to the evt files on the SeaFlow 
cruise.id <- 'july2014' # Cruise name



###################
### DON'T TOUCH ###
###################

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

# name of SQLite database containing OPP and VCT tables
db.name <- '~/popcycle/sqlite/popcycle.db'
opp.table.name <- 'opp'
vct.table.name <- 'vct'
opp.evt.ratio.table.name <- 'opp_evt_ratio'
stats.table.name <- 'stats'

# location to log filtering and gating parameters
log.location <- '~/popcycle/logs'
log.gate.location <- paste(log.location, 'gates', sep='/')
log.filter.location <- paste(log.location, 'filter', sep='/')

# location of parameters for filter and gating 
param.location <- '~/popcycle/params'
param.gate.location <- paste(param.location, 'gates', sep='/')
param.filter.location <- paste(param.location, 'filter', sep='/')



# flow calibration for SEAFLOW1
# ratio.evt.stream = 0.14756
# x <- 12 #psi
# flow_rate <- (-9*10^-5 * x^4 + 0.0066 * x^3 - 0.173 * x^2 + 2.5013 * x + 2.1059)  *  ratio.evt.stream # where x is STREAM_PRESSURE (psi)
