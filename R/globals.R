# Configuration definitions

# standard header for EVT files, needed for readSeaflow
EVT.HEADER <- c("time","pulse_width","D1","D2",
                "fsc_small","fsc_perp","fsc_big","pe","chl_small","chl_big")
CHANNELS <- EVT.HEADER[3:length(EVT.HEADER)]  # optical data channels
EVT.HEADER2 <- c("pulse_width", "chl_small", "D1", "D2", "fsc_small", "pe", "evt_rate")
CHANNELS2 <- EVT.HEADER2[2:(length(EVT.HEADER2)-1)]  # optical data channels

# Quantiles to use for filtering
QUANTILES <- c(2.5, 50.0, 97.5)
# Flag values for OPP quantiles
QFLAGS <- c(1,2,4)
names(QFLAGS) <- c("q2.5", "q50", "q97.5")

# Population names
POPNAMES <- c("beads", "croco", "picoeuk", "prochloro", "synecho", "unknown")

# Outlier flag meanings
FLAG_OK <- 0L          # not an outlier
FLAG_INSTRUMENT <- 1L  # stream pressure, event rate
FLAG_FILTRATION <- 2L  # OPP:EVT, bead scatter
FLAG_GATING <- 3L      # gating outliers

# opp2 table flag meanings
FLAG_OPP2_OK <- 0L             # file is OK
FLAG_OPP2_EMPTY <- 1L          # empty or unreadable file
FLAG_OPP2_EVT_HIGH <- 2L       # too many EVT events
FLAG_OPP2_OPP_HIGH <- 3L       # too many OPP events
FLAG_OPP2_EMPTY_QUANTILE <- 4  # at least one quantile has no OPP particles

# Maximum particles per 3-minute EVT file
MAX_PARTICLES_PER_FILE_DEFAULT <- 50000 * 180  # max event rate (per sec) 50k
# Maximum OPP particles per file
MAX_OPP_PARTICLES_PER_FILE_DEFAULT <- 20000

instrument.id <- "740"

# This package's name
.pkg.name <- "popcycle"

# Import symbols to enable convenient syntax in dplyr, data.table, and foreach
#' @importFrom magrittr "%>%"
#' @importFrom data.table .N
#' @importFrom foreach "%dopar%"
1