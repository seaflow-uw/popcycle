# Configuration definitions

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

opp.table.name <- 'opp'
vct.table.name <- 'vct'
opp.evt.ratio.table.name <- 'opp_evt_ratio'
stats.table.name <- 'stats'
cytdiv.table.name <- 'cytdiv'
sfl.table.name <- 'sfl'

default.project.location <- paste('~/popcycle') # Default project directory location
default.evt.location <- '~/SeaFlow/datafiles/evt' # Path to the evt files for the real-time analysis
default.instrument.location <- '/Volumes/evt' # Path to the evt files on the SeaFlow 
default.cruise.id <- 'july2014' # Cruise name

# Configure project locations based on top-level directory path.  Place
# location definitions in the global environment.  Create project directory
# structure and database file if necessary.
#
# Args:
#   path = top-level directory for popcycle project
# 
# TODO (CTB):
#   * Move variables defined in this function to the package namespace
set.project.location <- function(path=default.project.location) {
  assign("project.location", path.expand(path), 1)

  # location to log filtering and gating parameters
  assign("log.location", paste(project.location, 'logs', sep='/'), 1)
  assign("log.gate.location", paste(log.location, 'gates', sep='/'), 1)
  assign("log.filter.location", paste(log.location, 'filter', sep='/'), 1)

  # location of parameters for filter and gating 
  assign("param.location", paste(project.location, 'params', sep='/'), 1)
  assign("param.gate.location", paste(param.location, 'gates', sep='/'), 1)
  assign("param.filter.location", paste(param.location, 'filter', sep='/'), 1)

  # folder for sqlite db files
  assign("db.location", paste(project.location, 'sqlite', sep='/'), 1)
  # name of SQLite database containing OPP and VCT tables
  assign("db.name", paste(db.location, 'popcycle.db', sep="/"), 1)

  if (! file.exists(project.location)) {
    dir.create(project.location)
    dir.create(log.location)
    dir.create(log.gate.location)
    dir.create(log.filter.location)
    dir.create(param.location)
    dir.create(param.gate.location)
    dir.create(param.filter.location)
    dir.create(db.location)
    reset.db(db.location)
  }
}

# Configure EVT file location.  Place location definition in the global
# environment.
#
# Args:
#   path = top-level directory for EVT files
# 
# TODO (CTB):
#   * Move variables defined in this function to the package namespace
set.evt.location <- function(path=default.evt.location) {
  assign("evt.location", path.expand(path), 1)
}

# Configure cruise name.  Place location definition in the global
# environment.
#
# Args:
#   cruise.name = cruise name
# 
# TODO (CTB):
#   * Move variables defined in this function to the package namespace
set.cruise.id <- function(cruise.name=default.cruise.id) {
  assign("cruise.id", path.expand(cruise.name), 1)
}

# Configure instrument directory location.  Place location definition in the
# global environment.
#
# Args:
#   instrument.location = path to instrument directory
#
# TODO (CTB):
#   * Move variables defined in this function to the package namespace
set.instrument.location <- function(instrument.loc=default.instrument.location) {
  assign("instrument.location", path.expand(instrument.loc), 1)
}

# flow calibration for SEAFLOW1
# ratio.evt.stream = 0.14756
# x <- 12 #psi
# flow_rate <- (-9*10^-5 * x^4 + 0.0066 * x^3 - 0.173 * x^2 + 2.5013 * x + 2.1059)  *  ratio.evt.stream # where x is STREAM_PRESSURE (psi)
