# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

evt.location <- '~/SeaFlow/datafiles/evt/'

# name of SQLite database containing OPP and VCT tables
db.name <- '~/popcycle/sqlite/popcycle.db'
opp.table.name <- 'opp'
vct.table.name <- 'vct'
cruise.id <- 'july2014'

# location of parameters for filter and gating params
filter.param.location <- '~/popcycle/params/filter/'
gating.param.location <- '~/popcycle/params/gates/'

# location of parameters for filter and gating params
filter.param.location_archived <- '~/popcycle/params/filter_archived/'
gating.param.location_archived <- '~/popcycle/params/gates_archived/'
