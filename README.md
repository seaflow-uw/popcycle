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
1. The first step is to indicate where the database (`popcycle.db`) is located and where to save the project

    ```r
    library(popcycle) 
    set.cruise.id("foo")
    set.evt.location("/path/to/evt/files") # e.g., "/Volumes/cruise.id/evt"
    set.project.location("/path/to/project") # e.g., "~/Cruise.id_project"
    ```

2. The second step is to set the parameters for the filtration method, i.e., the `width`(to adjust the alignment of the instrument) and the `notch` (to adjust the focus of the instrument) . The `notch` represents the the ratio D/fsc_small and  depends on how the PMTs of D1/D2 and fsc_small were set up, the `width` represents the acceptable difference between D1 and D2 for a particle to be considered 'aligned', it is usually set between 0.1 and 0.5. For this example, we are going to choose the `notch` using the latest evt file collected by the instrument (but you choose any evt file that you want, of course). The `width` is  set to 0.2. Open an R session and type:

    ```r
    # name of the latest evt file collected
    file.name <- get.latest.evt.with.day() 
    # load evt file
    evt <- readSeaflow(paste(evt.location, file.name, sep='/')) # load the evt file
    # Set the width and find the best notch parameter
    width <- 0.2
    notch <- best.filter.notch(evt, notch=seq(0.5, 1.5, by=0.1),width=width, do.plot=TRUE)
    # to plot the filtration step, use the following function
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


2. Second step is to set the gating for the different populations. WARNINGS: The order in which you gate the different populations is very important, choose it wisely. The gating has to be performed over optimally positioned particles only, not over an evt file. In this example, you are going to first gate the 'beads' (this is always the first population to be gated.). Then we will gate 'Synechococcus' population (this population needs to be gated before you gate Prochlorococcus or picoeukaryote), and finally 'Prochlorococcus' and 'picoeukaryote' population. After drawing your gate on the plot, right-click to finalize.
In the R session, type:

    ```r
    file.name <- get.latest.evt() # name of the latest evt file collected
    opp <- get.opp.by.file(file.name)
    setGateParams(opp, popname='beads', para.x='fsc_small', para.y='pe')
    setGateParams(opp, popname='synecho', para.x='fsc_small', para.y='pe')
    setGateParams(opp, popname='prochloro', para.x='fsc_small', para.y='chl_small')
    setGateParams(opp, popname='picoeuk', para.x='fsc_small', para.y='chl_small')
    ```

    Similar to the `setFilterParams` function, `setGateParams` saves the gating parameters and order in which the gating was performed in `~/popcycle/params/params.RData`, parameters for each population are also separately saved as a `.csv` file. Note that every changes in the gating parameters are automatically saved in the logs (`~popcycle/logs/params/`).

    Note: If you want to change the order of the gating, delete a population, or simply restart over, use the function 
    <code>resetGateParams()</code>
    
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
   run.filter(start.day, start.timestamp, end.day, end.timestamp)
   ```
   
 where `start.day` and `end.day` represent the folder name (year_julianday) and `start.timestamp` and `end.timestamp` the file name (ISO8601) of the first and last file you want to reanalyze. This function will create/update the 'opp' table in the database.

2. To apply the gating parameters and analyze opp files according to gating parameters, use the following function

   ```r
   run.gating(start.day, start.timestamp, end.day, end.timestamp)
   ```

This function will create/update the 'vct' and 'stats' table.

# Visualization
Data generated can be visualize using a set of functions:

1. To plot the filter steps

    ```r
    plot.filter.cytogram.by.file(file.name)
    ```

2. To plot opp

    ```r
    plot.cytogram.by.file(file.name)
    ```

3. To plot vct

    ```r
    plot.vct.cytogram.by.file(file.name)
    ```

4. To plot aggregate statistics, for instance, cell abundance the cyanobacteria "Synechococcus" population on a map or over time

    ```r
    stat <- get.stat.table() # to load the aggregate statistics
    plot.map(stat, pop='synecho', param='abundance') 
    plot.time(stat, pop='synecho', param='abundance')
    ```

    But you can plot any parameter/population, just make sure their name match the one in the 'stat' table... 

    FYI, type `colnames(stat)` to know which parameters are available in the 'stat' table,  and `unique(stat$pop)` to know the name of the different populations.
