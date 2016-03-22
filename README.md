Popcycle
========
The Popcycle pipeline performs 3 different analyses:

1. Filtration of **O**ptimally **P**ositioned **P**articles (**OPP**)
2. Manual Gating of cytometric populations (**VCT**)
3. Aggregate statistics for the different populations.

The metadata and aggregated statistics for each step are saved into a SQL database using SQLite3 and the particle data is saved into gzip compressed binary and text files.


# Installation
First we need to satisfy some dependencies. Make sure Python 2.7 and `pip` is installed, then install the Python `dateutil` library. If you're using Anaconda then `dateutil` is already installed.

```
pip install python-dateutil
```

Now download and install the popcycle R library

```
git clone https://github.com/uwescience/popcycle
cd popcycle
# This will install popcycle and automatically run some tests
Rscript setup.R
```

## Help
Documentation for `popcycle` functions used in this guide can be accessed with the built-in R help system. e.g. running the following command in R will bring up a separate doc window for the `filter.evt.files` function.

```r
?filter.evt.files
```

## Initialization
First start R and load the popcycle library

```r
library(popcycle)
```

Next, set a few variables which will define input and output file locations as well a cruise name. We'll call this cruise "testcruise" and use a small test SeaFlow data set that was installed with popcycle.

```r
cruise <- "testcruise"
db <- paste0(cruise, ".db")

# For this guide we'll use an example data set that's installed
# with popcycle, but for real analysis this path would point to
# an EVT directory for a real cruise.
evt.dir <- system.file("extdata/SeaFlow/datafiles/evt", package="popcycle")

opp.dir <- paste0(cruise, "_opp")
vct.dir <- paste0(cruise, "_vct")
```

`db` is the SQLite3 database file which will store aggregate statistics for **OPP** and **VCT** files as well as the filtering and gating parameters used to generate **OPP** and **VCT** data.  
`evt.dir` is the input directory containing **EVT** files of raw, unfiltered particle data.  
`opp.dir` is the output directory which will contain **OPP** files of filtered particle data.  
`vct.dir` is the output directory which will contain **VCT** files of per particle population classifications

Create the popcycle `SQLite3` database. If you're using a database that already exists - for example if you filtered using `filterevt.py` from the [seaflowpy](https://github.com/armbrustlab/seaflowpy) project - this step isn't necessary, but won't hurt anything either.

```r
make.popcycle.db(db)  # Create a popcycle SQLite3 database file
```

For many database operations it's necessary to access the cruise information contained in SFL files, so let's load that now. We can load all SFL files found in the EVT directory or load from a single concatenated SFL file.

```r
save.sfl(db, cruise, evt.dir=evt.dir)
# OR from a single file
# save.sfl(db, cruise, sfl.file=sfl.file)
get.sfl.table(db)  # View SFL table to confirm import
```

## Note on file names

SeaFlow data is intially saved as EVT binary files representing three-minute windows of time. For example, in our example data set the EVT files within `evt.dir` are:

```
2014_185/2014-07-04T00-00-02+00-00.gz
2014_185/2014-07-04T00-03-02+00-00.gz
2014_185/2014-07-04T00-06-02+00-00.gz
2014_185/2014-07-04T00-09-02+00-00.gz
2014_185/2014-07-04T00-12-02+00-00.gz
```

This is actually a file path that includes the julian day directory, but it will be often be referred to as the file name in `popcycle`. A slightly modified version of this file name (no .gz extension) will be used to identify associated entries in the SQLite3 database and to name derived files for OPP and VCT data.

For example, the first file `2014_185/2014-07-04T00-00-02+00-00.gz` would have a `file` field value of `2014_185/2014-07-04T00-00-02+00-00` in any database tables. The derived OPP and VCT files would be named `2014_185/2014-07-04T00-00-02+00-00.opp.gz` and `2014_185/2014-07-04T00-00-02+00-00.vct.gz` within the `opp.dir` and `vct.dir` output directories.

Any version of the file name can be converted to the short version used by `popcycle` functions with `clean.file.path`.

## Filtering

### Fast filtering with Python
The fastest way to filter EVT files is to use `filterevt.py` from the [seaflowpy](https://github.com/armbrustlab/seaflowpy) project. This will create filtered OPP file and database output equivalent to the R code in this section, but could potentially save you hours or days of time. For example, to filter an EVT directory called `testcruise_evt` using 4 threads:

```sh
filterevt.py --evt_dir testcruise_evt --cpus 4 --cruise testcruise --db testcruise.db --opp_dir testcruise_opp
```

This will result in a new directory `testcruise_opp` with OPP files and a new database `testcruise.db` containing filter parameters and OPP aggregated statistics. For full usage information run

```sh
filterevt.py -h
```

If you go this route skip ahead to the section on Gating.

### Configure filter parameters
Set parameters for filtration and filter raw data to create OPP. In most cases it's sufficient to use default parameters.

```r
save.filter.params(db)  # Save default filter parameters
get.filter.params.latest(db)  # Examine the default parameters we just set
```

This saves a log entry of new filter parameters. To set different parameters run `save.filter.params` again with custom parameters. Each time this function is run a new filter parameter entry is made in the database. By default the latest parameters are used for filtering, but it's possible to use a specific parameter set by filter ID.

To view all filter parameter entries and find filter IDs run

```r
get.filter.table(db)
```

### Filter particles

Now we'll filter EVT files to create OPP data.

```r
evt.files <- get.evt.files(evt.dir)  # Find 5 EVT files in evt.dir
save.filter.params(db)  # Save default filter parameters
get.filter.params.latest(db)  # Examine the parameters we just set
# Filter particles
filter.evt.files(db, cruise, evt.dir, evt.files, opp.dir)
# 2 of the 5 input EVT files are invalid and will produce warning
# messages here which can be ignored.
```

There should be three new OPP files of filtered particles in the `testcruise_opp` directory.

### Filtering subsets of EVT files

It is sometimes desirable to apply different filter parameters to different groups of EVT files. The filter parameters to use can be specified by the `filter.id` parameter to `filter.evt.files`.

```r
# If you don't want to use the latest filter parameters, pass a
# filter ID retrieved from `get.filter.table(db)` to filter.evt.files
# e.g.
filter.evt.files(db, cruise, evt.dir, evt.files, opp.dir,
                 filter.id="d3afb1ea-ad20-46cf-866d-869300fe17f4")
```

To get a subset of EVT files selected by date, use `get.evt.files.by.date`.

## Gating

### Configure gating parameters
Now we're ready to to set the gating for the different populations and classify particles.

**WARNING**: The order in which you gate the different populations is very important, choose it wisely. The gating has to be performed over optimally positioned particles (OPP) only, not over an EVT file.

In this example, you are going to first gate the `beads` (this is always the first population to be gated.). Then we will gate the `Synechococcus` population (this population needs to be gated before you gate `Prochlorococcus` or `Picoeukaryote`), and finally the `Prochlorococcus` and `Picoeukaryote` populations.

We'll use all three files of the example data set to configure gating parameters.

```r
opp.files <- get.opp.files(db)  # 3 OPP file names
opp <- get.opp.by.file(opp.dir, opp.files)
poly.log <- set.gating.params(opp, "beads", "fsc_small", "pe")
```

This will open an R plot for a cytogram of forward scatter versus phycoerythrin. Left click locations in the cytogram to define a polygon to classify bead particles. To close the last segment and finalize the polygon right-click a location in the cytogram, then close the plot window.

![gating cytogram for bead](documentation/images/beads-gate.png?raw=true)

We'll follow the same process to continue to append to `poly.log`, creating gates for synechococcus, prochlorococcus, and picoeukaryotes.

```r
# Note: we pass in an already existing poly.log to append new gates
poly.log <- set.gating.params(opp, "synecho", "fsc_small", "pe", poly.log)
poly.log <- set.gating.params(opp, "prochloro", "fsc_small", "chl_small", poly.log)
poly.log <- set.gating.params(opp, "picoeuks", "fsc_small", "chl_small", poly.log)
```

Your final gating polygon might look something like this

```r
plot.gating.cytogram(opp, poly.log, "fsc_small", "pe")
plot.gating.cytogram(opp, poly.log, "fsc_small", "chl_small")
```

![gating cytogram for fsc_small versus pe](documentation/images/fsc_small-pe-gates.png?raw=true)
![gating cytogram for fsc_small versus chl_small](documentation/images/fsc_small-chl_small-gates.png?raw=true)

Now save the gating parameters to the database

```r
save.gating.params(db, poly.log)
```

Similar to the `save.filter.params` function, `save.gating.params` saves the gating parameters and order in which the gating was performed. Every call to `save.gating.params` creates a new gating entry in the database which can be retrieved by ID. To get a summary of gating entries and find gating IDs run

```r
get.gating.table(db)
```

and to retrieve polygon parameters by ID run

```r
# Where gating.id is ID text found in a get.gating.table() data frame
get.gating.params.by.id(db, gating.id)
```

### Classify particles
To cluster the different population according to your manual gating, run:

```r
classify.opp.files(db, cruise, opp.dir, opp.files, vct.dir)
```

There will be three new VCT files in `testcruise_vct` directory. We can examine classifications for all particles with `plot.vct.cytogram`.

```r
# Note: we pass vct.dir here to color particles by our new classifications
opp <- get.opp.by.file(opp.dir, opp.files, vct.dir=vct.dir)
plot.vct.cytogram(opp, "fsc_small", "pe")
plot.vct.cytogram(opp, "fsc_small", "chl_small")
```

![VCT cytogram for fsc_small versus pe](documentation/images/fsc_small-pe-vct.png?raw=true)
![VCT cytogram for fsc_small versus chl_small](documentation/images/fsc_small-chl_small-vct.png?raw=true)

### Classify subsets of EVT files

Just as in filtering, it is sometimes desirable to apply different gating parameters to different groups of OPP files. The gating parameters to use can be specified by the `gating.id` parameter to `classify.opp.files`.

```r
# If you don't want to use the latest gating parameters, pass a
# gating ID retrieved from `get.gating.table(db)` to classify.opp.files
# e.g.
classify.opp.files(db, cruise, opp.dir, opp.files, vct.dir,
                   gating.id="c3a06970-3552-4c1c-a71d-f71af93f4d60")
```

To get a list of OPP files selected by date, use the `file` column of the data frame returned by `get.opp.stats.by.date`.


## Summary statistics
Once clustering has finished you can view aggregate statistics for the whole cruise with

```r
get.stat.table(db)
```


## Visualization

Data can be plotted using a set of functions:

1. To plot the filter steps

    ```r
    set.evt.location("/path/to/evt/files")
    evt.list <- get.evt.list()
    evt.name <- evt.list[10] # to select to 10th evt file of the list
    plot.filter.cytogram.by.file(evt.name,width=0.2, notch=1)
    ```

2. To plot an evt cytogram. WARNING: the number of particles in an evt file can be high (>10,000) which can be a problem for some computer. We advise to limit the disply to < 10,000 particles.

    ```r
    set.evt.location("/path/to/evt/files")
    evt.list <- get.evt.list()
    evt.name <- evt.list[10] # to select to 10th evt file of the list
    evt <- readSeaflow(evt.name)
    # TO LIMIT the number of displyed particles to 10,000
    if(nrow(evt) > 10000) evt <- evt[round(seq(1,nrow(evt), length.out=10000)),]
    plot.cytogram(evt, "fsc_small","chl_small")
    ```

3. To plot an opp cytogram

    ```r
    set.project.location("/path/to/project") # e.g., "~/Cruise.id_project"

    # OPTION 1: SELECT OPP data by FILES
    opp.list <- get.opp.files()
    opp.name <- opp.list[10] # to select the opp files (e.g., the 10th opp file in the list, corresponding to 9 minutes of data)
    opp <- get.opp.by.file(opp.name)
    plot.cytogram(opp, "fsc_small","chl_small)
    # OR DIRECTLY
    plot.cytogram.by.file(opp.name, "fsc_small","chl_small)

    # OPTION 2: SELECT OPP data by DATE
    sfl <- get.sfl.table()
    sfl$date <- as.POSIXct(sfl$date,format="%FT%T",tz='GMT')
    opp <- get.opp.by.date(sfl$date[1], sfl$date[1]+60*60, pop=NULL, channel=NULL) # e.g., select 1-h of data
    plot.cytogram(opp, "fsc_small","chl_small)
    ```

4. To plot an opp cytogram with clustered populations

    ```r
    set.project.location("/path/to/project") # e.g., "~/Cruise.id_project"
    opp.list <- get.opp.files()
    opp.name <- opp.list[10] # to select the opp file (e.g., the 10th opp file in the list)
    plot.vct.cytogram.by.file(opp.name)
    ```

5. To plot aggregate statistics, e.g., cell abundance the cyanobacteria `Synechococcus` population on a map or over time

    ```r
    set.project.location("/path/to/project") # e.g., "~/Cruise.id_project"
    stat <- get.stat.table() # to load the aggregate statistics
    plot.map(stat, pop='synecho', param='abundance')
    plot.time(stat, pop='synecho', param='abundance')
    ```

    But you can plot any parameter/population, just make sure their name match the one in the 'stat' table...

    FYI, type `colnames(stat)` to know which parameters are available in the `stat` table,  and `unique(stat$pop)` to know the name of the different populations.

6. Data stored in the popcycle.db can be visualized directly in R. Here is an example to display the first 10 row of the opp table in popcycle.db

    ```r
    set.project.location("/path/to/project") # e.g., "~/Cruise.id_project"
    conn <- dbConnect(SQLite(), dbname = db.name)
    dbGetQuery(conn, "SELECT * FROM opp LIMIT 10;")
    ```
