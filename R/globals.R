# Configuration definitions

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

# DB table names
opp.table.name <- 'opp'
vct.table.name <- 'vct'
opp.evt.ratio.table.name <- 'opp_evt_ratio'
stats.table.name <- 'stats'
cytdiv.table.name <- 'cytdiv'
sfl.table.name <- 'sfl'

# Data locations and cruise identifier
project.location <- paste('~/popcycle') # Default project directory location
evt.location <- '~/SeaFlow/datafiles/evt' # Path to the evt files for the real-time analysis
instrument.location <- '/Volumes/evt' # Path to the evt files on the SeaFlow 
cruise.id <- 'july2014' # Cruise name

# This package's name
.pkg.name <- 'popcycle'

# Note about configuration variables (globals)
##############################################
# Configuration variables are assigned to both the namespace and package
# environments through the set.* funcitons below.  This exposes their
# values to both the user and package functions by using the same names, 
# e.g. log.location has the same value for both the user and the package
# developer.  Hopefully this limits confusion.  However, it's still
# possible for the user to assign a value to log.location in the global
# environment which would mask the exported package variable, which is
# why the setter functions are provided to make sure assignments happen
# in the correct environments.

# Configure project locations based on top-level directory path.
# Create project directory structure and database file if necessary.
#
# Args:
#   path = top-level directory for popcycle project
set.project.location <- function(path) {
  .assign.to.envs("project.location", path.expand(path))

  # location to log filtering and gating parameters
  .assign.to.envs("log.location", paste(project.location, 'logs', sep='/'))
  .assign.to.envs("log.gate.location", paste(log.location, 'gates', sep='/'))
  .assign.to.envs("log.filter.location", paste(log.location, 'filter', sep='/'))

  # location of parameters for filter and gating 
  .assign.to.envs("param.location", paste(project.location, 'params', sep='/'))
  .assign.to.envs("param.gate.location", paste(param.location, 'gates', sep='/'))
  .assign.to.envs("param.filter.location", paste(param.location, 'filter', sep='/'))

  # folder for sqlite db files
  .assign.to.envs("db.location", paste(project.location, 'sqlite', sep='/'))
  # name of SQLite database containing OPP and VCT tables
  .assign.to.envs("db.name", paste(db.location, 'popcycle.db', sep="/"))

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
set.evt.location <- function(path) {
  .assign.to.envs("evt.location", path.expand(path))
}

# Configure cruise name.  Place location definition in the global
# environment.
#
# Args:
#   cruise.name = cruise name
set.cruise.id <- function(cruise.name) {
  .assign.to.envs("cruise.id", path.expand(cruise.name))
}

# Configure instrument directory location.  Place location definition in the
# global environment.
#
# Args:
#   instrument.location = path to instrument directory
set.instrument.location <- function(instrument.loc) {
  .assign.to.envs("instrument.location", path.expand(instrument.loc))
}

# Assign a value to a symbol in both the namespace and package environment.
# If package environment is not available only assign to namespace.  If the
# binding for that symbol is locked, unlock it first then lock it after
# assignment.
# 
# Args:
#   sym = a name object or character string
#   val = the value to assign
.assign.to.envs <- function(sym, val) {
  envs <- .get.envs()
  for (e in c(envs$namespace, envs$package)) {
    lock = FALSE
    if (exists(sym, e)) {
      if (bindingIsLocked(sym, e)) {
        lock = TRUE
        unlockBinding(sym, e)
      }
    }
    assign(sym, val, e)
    if (lock) {
      lockBinding(sym, e)
    }
  }
}

# Attempt to get the namespace and package environments as a two item
# named list with elements "namespace" and "package".  If package isn't
# available (as in .onLoad) its values is set to NULL.
.get.envs <- function() {
  envs <- list('namespace'=NULL, 'package'=NULL)
  envs$namespace <- parent.env(environment())
  tryCatch({
    envs$package <- as.environment(paste0('package:', .pkg.name))
  }, error = function(err) {
    # Do nothing
  })
  return(envs)
}

# flow calibration for SEAFLOW1
# ratio.evt.stream = 0.14756
# x <- 12 #psi
# flow_rate <- (-9*10^-5 * x^4 + 0.0066 * x^3 - 0.173 * x^2 + 2.5013 * x + 2.1059)  *  ratio.evt.stream # where x is STREAM_PRESSURE (psi)
