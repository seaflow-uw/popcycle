## Installation

Download and install the popcycle R library

```sh
git clone https://github.com/uwescience/popcycle
cd popcycle
Rscript setup.R
```

## Standard workflow

First start R and load the popcycle library
```R
library(popcycle)
```

Next, set a few variables which will define input and output file locations as well a cruise name. We'll call this cruise "testcruise" and use the test SeaFlow data set that was installed with popcycle.

```R
cruise <- "testcruise"
db <- paste0(cruise, ".db")
evt.dir <- system.file("extdata/SeaFlow/datafiles/evt", package="popcycle")
opp.dir <- paste0(cruise, "_opp")
vct.dir <- paste0(cruise, "_vct")
```

`db` is the SQLite3 database file which will store aggregate statistics for OPP and VCT files as well as the filtering and gating parameters used to generate OPP and VCT data.  
`evt.dir` is the input directory containing EVT files of raw, unfiltered particle data.  
`opp.dir` is the output directory which will contain OPP files of filtered particle data.  
`vct.dir` is the output directory which will contain VCT files of per particle population classifications

Create the new popcycle SQLite3 database and populate it with cruise information from SFL files found in the EVT directory. Here we assume the local popcycle git repository is in the directory `./popcycle`.

```R
make.popcycle.db(db)  # Create an empty popcycle SQLite3 database file
# save.sfl uses a python script in the git repository
popcycle.git.dir <- "./popcycle"
save.sfl(db, popcycle.git.dir, cruise, evt.dir=evt.dir)
get.sfl.table(db)  # View SFL table to confirm import
```

Next let's filter the raw EVT files to create OPP files.

```R
evt.files <- get.evt.files(evt.dir)  # Find 5 EVT files in evt.dir
save.filter.params(db)  # Save default filter parameters
get.filter.params.latest(db)  # Examine the default parameters we just set
filter.evt.files(db, cruise, evt.dir, evt.files, opp.dir)  # Filter particles
# 2 of the 5 input EVT files are invalid and will produce warning
# messages here which can be ignored.
```

Now you'll find three new OPP files of filtered particles in `testcruise_opp`.  

Next we'll set some population gating parameters based on the first OPP file and then use these parameters to classify particles in all three OPP files.

```R
opp.files <- get.opp.files(db)  # 3 OPP file names
opp <- get.opp.by.file(opp.dir, opp.files[1])
poly.log <- set.gating.params(opp, "beads", "fsc_small", "pe")
```
This will open an R plot for a cytogram of forward scatter versus phycoerythrin. Left click locations in the cytogram to define a polygon to classify bead particles. To close the last segment of the polygon right-click a location in the cytogram, then close the plot window.

![gating cytogram for bead](images/beads-gate.png?raw=true)

We'll follow the same process to continue to append to `poly.log`, creating gates for synechococcus, prochlorococcus, and picoeukaryotes.

```R
poly.log <- set.gating.params(opp, "synecho", "fsc_small", "pe", poly.log)
poly.log <- set.gating.params(opp, "prochloro", "fsc_small", "chl_small", poly.log)
poly.log <- set.gating.params(opp, "picoeuks", "fsc_small", "chl_small", poly.log)
```

Your final gating polygon might look something like this

```R
plot.gating.cytogram(opp, poly.log, "fsc_small", "pe")
plot.gating.cytogram(opp, poly.log, "fsc_small", "chl_small")
```
![gating cytogram for fsc_small versus pe](images/fsc_small-pe-gates.png?raw=true)
![gating cytogram for fsc_small versus chl_small](images/fsc_small-chl_small-gates.png?raw=true)

Now save the gating parameters to the database and classify particles in all OPP files.

```R
save.gating.params(db, poly.log)
run.gating(db, cruise, opp.dir, opp.files, vct.dir)
```

There will be three new VCT files in `testcruise_vct`. We can examine classifications for all particles with `plot.vct.cytogram`.

```R
# Note: we pass vct.dir here to get our new per-particle population annotations
opp <- get.opp.by.file(opp.dir, opp.files, vct.dir=vct.dir)
plot.vct.cytogram(opp, "fsc_small", "pe")
plot.vct.cytogram(opp, "fsc_small", "chl_small")
```

![VCT cytogram for fsc_small versus pe](images/fsc_small-pe-vct.png?raw=true)
![VCT cytogram for fsc_small versus chl_small](images/fsc_small-chl_small-vct.png?raw=true)

Use `get.stat.table` to generate summary statistics for the whole cruise.

```R
get.stat.table(db)
```
