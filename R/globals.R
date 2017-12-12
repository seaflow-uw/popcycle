# Configuration definitions

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

# Quantiles to use for filtering
QUANTILES <- c(2.5, 50.0, 97.5)

instrument.id <- '740'

# This package's name
.pkg.name <- 'popcycle'
