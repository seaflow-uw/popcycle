Popcycle - A reproducible approach to process, calibrate and curate shipboard underway flow cytometry data
========
**Popcycle** is an R package that uses a database management system approach to analyze flow cytometry data collected by SeaFlow. 

<img src="https://github.com/armbrustlab/seaflow-sfl/blob/master/cruise-track.png" alt="SeaFlow cruisetracks"	title="SeaFlow cruisetracks" align="left" style="float" width="400">
SeaFlow is an shipboard underway flow cytometer that provides continuous optical measurements of light scatter and fluorescence associated with the pigments chlorophyll a and phycoerythrin at the single cell level [(Swalwell et al. 2011)](https://doi.org/10.4319/lom.2011.9.466). Over the last decade, the instrument has measured the optical properties of over 300 billions small individual phytoplankton cells (< 5 $\mu$m in diameter) across the North Pacific Ocean at a spatial resolution of ~ 1 km along the cruise track. We have created Popcycle, an R package that use new reproducible analytical methods to uniformely process, calibrate and curate SeaFlow data. 

SeaFlow data are stored every 3 minutes in a custom binary file format (RAW data) consisting of eight 16-bit integer channels. The acquisition time, stream pressure of each sample, and contextual information provided by the ship's data system (e.g., time, location, sea surface temperature, salinity, light intensity) are written into a log file ([metadata](https://github.com/armbrustlab/seaflow-sfl)). The files are stored in day-of-year-labeled directories, each containing raw files with the associated log file.

<img src="documentation/images/seaflow-workflow.png?raw=true" alt="Popcycle workflow"
	title="Popcycle workflow" align="right" style="float" width="500">

**Popcycle** performs 3 key analyses:
1. ```Gating```: Classification of phytoplankton cell populations using a mixture of manual gating and a semi-supervized clusterting algorithm.
2. ```Light scatter conversion```: Convert light scattering of each particle to [carbon content](https://github.com/armbrustlab/fsc-poc-calibration) and [cell diameter](https://github.com/armbrustlab/fsc-size-calibration).
3. ```PER POPULATION data```: Perform aggregate statistics along with error propagation for the different populations.
The cell population identification (GATED data) and diameter and carbon content (CALIBRATED data) of each OPP are saved as separate text files with a similar file structure as the RAW data.

The metadata, gating scheme, and aggregated statistics for each step are saved to a SQL database using SQLite3.

### Install
In this project's directory, run the install script `setup.R`. This will install `devtools`, `BiocManager`, and any dependencies listed in `DESCRIPTION`. It will not upgrade any packages. To upgrade dependencies, manually run the install commands in `setup.R` in an interactive R environment making sure to set install options for upgrading or updating appropriately.

```
Rscript setup.R
```

### Analysis
To get started with **Popcycle** analysis, go to the [wiki](https://github.com/armbrustlab/popcycle/wiki/SeaFlow-data-analysis-tutorial)
