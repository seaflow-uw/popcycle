# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

cruise.id <- 'july2014'
  
# name of SQLite database containing OPP and VCT tables

# TODO(hyrkas): make sure location is constant
db.name <- '~/popcycle/sqlite/popcycle.db'
opp.table.name <- 'opp'
vct.table.name <- 'vct'

# location to log filtering and gating parameters

# TODO(hyrkas): log params in database?
log.location <- '~/popcycle/logs/'

# location of parameters for filter and gating params
filter.param.location <- '~/popcycle/params/filter.csv'
gating.param.location <- NULL # TODO(hyrkas): define