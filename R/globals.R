# Configuration definitions

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")

# Quantiles to use for filtering
QUANTILES <- c(2.5, 50.0, 97.5)

# Outlier flag meanings
FLAG_OK <- 0          # not an outlier
FLAG_INSTRUMENT <- 1  # stream pressure, event rate
FLAG_FILTRATION <- 2  # OPP:EVT, bead scatter
FLAG_GATING <- 3      # gating outliers

instrument.id <- '740'

# This package's name
.pkg.name <- 'popcycle'
