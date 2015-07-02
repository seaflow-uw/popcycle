Popcycle
========

![travis-ci status](https://travis-ci.org/uwescience/popcycle.svg?branch=master)

The Popcycle pipeline performs 3 different analyses:

1. filtration of Optimally Positioned Particles
2. Manual Gating of cytometric populations
3. Aggregate statistics for the different populations.

The output of each step is saved into a SQL database using `sqlite3`. To run `popcycle` and analyze SeaFlow data in real-time, you need to set the filter and gating parameters, and press play, that's it!

# Installation (only for OSX)
1. Clone the popcycle git repository (e.g., called `popcycle-master`, and avoid calling it just `popcycle`) into your computer . For that, open the terminal and type
    ```sh
    $ git clone https://github.com/uwescience/popcycle.git path/to/popcycle-master
    (replace path/to/popcycle-master by the actual path...)
    ```

2. Install popcycle package and its dependencies, such as `RSQLite` and `splancs` packages if they are not already installed

    ```sh
    $ cd path/to/popcyle-master
    $ Rscript setup.R
    ```
WARNINGS: You need to be in the popcycle repository to execute the setup.R script. The setup process creates a popcycle directory in `~/popcycle`, this is different from the popcycle repository.

# Initialization
1. The first step is to indicate where the raw files (`evt`) and database (`popcycle.db`) are located and where to save the project

    ```r
    library(popcycle)
    # Required
    set.cruise.id("foo")
    set.evt.location("/path/to/evt/files") # e.g., "/Volumes/cruise.id/evt"
    # Optional (defaults to ~/popcycle)
    set.project.location("/path/to/project") # e.g., "~/Cruise.id_project"
    ```
NOTE: `set.project.location` will create a new database if the `("/path/to/project")` does not already exist

2. The second step is to set the parameters for the filtration method, i.e., the `width`(to adjust the alignment of the instrument) and the `notch` (to adjust the focus of the instrument) . The `notch` represents the the ratio D/fsc_small and  depends on how the PMTs of D1/D2 and fsc_small were set up, the `width` represents the acceptable difference between D1 and D2 for a particle to be considered 'aligned', it is usually set between 0.1 and 0.5. For this example, we are going to choose the `notch` using the latest evt file collected by the instrument (but you choose any evt file that you want, of course). The `width` is  set to 0.2. Open an R session and type:

    ```r
    # SELECT AN EVT FILE
    evt.list <- get.evt.list() # to get the entire list of evt files
    evt.name <- evt.list[10] # then select the evt file (e.g., the 10th evt file in the list)
    # OR
    evt.name <- get.latest.evt.with.day() # to get the last evt file of the list

    # LOAD THE EVT FILE
    evt <- readSeaflow(evt.name)

    # SET the WIDTH and NOTCH parameter
    width <- 0.2 # usually between 0.1 and 0.5
    notch <- 1 # usually between 0.5 and 1
    plot.filter.cytogram(evt, notch=notch, width=width) # to plot the filtration steps
    ```

    NOTE that if you have trouble finding the optimal NOTCH, you can use the function `find.filter.notch()`
    ```r
    width <- 0.2
    notch <- find.filter.notch(evt, notch=seq(0.5, 1.5, by=0.1),width=width, do.plot=TRUE)
    plot.filter.cytogram(evt, notch=notch, width=width)
    ```

  Once you are satisfy with the filter parameters, you can filter `evt` to get `opp` by typing:

    ```r
    opp <- filter.notch(evt, notch=notch, width=width)
    ```


    IMPORTANT: To save the filter parameters so the filter parmaters will be apply to all new evt files, you need to call the function:

    ```r
    setFilterParams(width, notch)
    ```
This function saves the parameters in ~/popcycle/params/filter/filter.csv. Note that every changes in the filter parameters are automatically saved in the logs (~popcycle/logs/filter/filter.csv).


2. Third step is to set the gating for the different populations. WARNINGS: The order in which you gate the different populations is very important, choose it wisely. The gating has to be performed over optimally positioned particles only, not over an evt file. In this example, you are going to first gate the `beads` (this is always the first population to be gated.). Then we will gate `Synechococcus` population (this population needs to be gated before you gate `Prochlorococcus` or `picoeukaryote`), and finally `Prochlorococcus` and `picoeukaryote` population. After drawing your gate on the plot, right-click to finalize.
In the R session, type:

    ```r
    # OPTION 1: SELECT OPP data by FILES
    opp.list <- get.opp.files()
    opp.name <- opp.list[10] # to select the opp files (e.g., the 10th opp file in the list, corresponding to 9 minutes of data)
    opp <- get.opp.by.file(opp.name)

    # OPTION 2: SELECT OPP data by DATE
    sfl <- get.sfl.table()
    sfl$date <- as.POSIXct(sfl$date,format="%FT%T",tz='GMT')
    opp <- get.opp.by.date(sfl$date[1], sfl$date[1]+60*60, pop=NULL, channel=NULL) # e.g., select 1-h of data

    # SET THE MANUAL GATING SCHEME
    setGateParams(opp, popname='beads', para.x='fsc_small', para.y='pe')
    setGateParams(opp, popname='synecho', para.x='fsc_small', para.y='pe')
    setGateParams(opp, popname='prochloro', para.x='fsc_small', para.y='chl_small')
    setGateParams(opp, popname='picoeuk', para.x='fsc_small', para.y='chl_small')
    ```

    Similar to the `setFilterParams` function, `setGateParams` saves the gating parameters and order in which the gating was performed in `~/popcycle/params/params.RData`, parameters for each population are also separately saved as a `.csv` file. Note that every changes in the gating parameters are automatically saved in the logs (`~popcycle/logs/params/`).

    Note: If you want to change the order of the gating, delete a population, or simply restart over, use the function
    `resetGateParams()`

3. To cluster the different population according to your manual gating, type:

    ```r
    vct <- classify.opp(opp, ManualGating)
    ```
4. To plot the cytogram with clustered populations, use the following function:

    ```r
    opp$pop <- vct
    par(mfrow=c(1,2))
    plot.vct.cytogram(opp, para.x='fsc_small', para.y='chl_small')
    plot.vct.cytogram(opp, para.x='fsc_small', para.y='pe')
    ```


# Play

1. To apply the filter parameters and analyze evt files according to the filter parameters, use the following function

   ```r
   evt.list <- get.evt.list()
   filter.evt.files(evt.list)
   ```
   NOTE that you can run `filter.evt.files` in a parallel fashion. If your computer has 4 cores, then type `filter.evt.files(evt.list, cores=4)`


2. To apply the gating parameters and analyze opp files according to gating parameters, use the following function

   ```r
   opp.list <- get.opp.files()
   run.gating(opp.list) # if you want to apply the gating scheme to ALL the available OPP data from the cruise
   ```

This function will create/update the `vct` and `stats` tables.

# Visualization

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
