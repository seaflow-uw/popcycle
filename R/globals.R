# Configuration definitions

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")
CHANNELS <- EVT.HEADER[3:length(EVT.HEADER)]  # optical data channels


# Quantiles to use for filtering
QUANTILES <- c(2.5, 50.0, 97.5)

# Flag values for OPP quantiles
QFLAGS <- c(1,2,4)
names(QFLAGS) <- c("q2.5", "q50", "q97.5")

instrument.id <- '740'

# This package's name
.pkg.name <- 'popcycle'
