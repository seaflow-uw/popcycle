# This function gets called when this package is loaded in R, e.g. library(pkgname).
# It performs initial setup of the project directory and configures the location of
# EVT files.
.onLoad <- function(libname, pkgname) {
    set.project.location()
    set.evt.location()
    set.cruise.id()
    set.instrument.location()
}
