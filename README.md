Popcycle
========
**Popcycle** is an R package that offers a reproducible approach to process, calibrate and curate flow cytometry data collected by SeaFlow. 

<img src="documentation/images/seaflow-workflow.png?raw=true" alt="Popcycle workflow"
	title="Popcycle workflow" align="right" style="float" width="500">
Raw  data are stored every 3 minutes in a custom binary file format (RAW data) consisting of eight 16-bit integer channels, along with [metadata](https://github.com/armbrustlab/seaflow-sfl) provided by the ship's data system (e.g., time, location, sea surface temperature, salinity, light intensity). The files are stored in day-of-year-labeled directories, each containing raw files with the associated log file. 

The software package performs 3 key analyses:
1. ```Gating```: Classification of phytoplankton cell populations using a mixture of manual gating and a semi-supervized clusterting algorithm.
2. ```Light scatter conversion```: Convert light scattering of each particle to cell diameter ([fsc-size-calibration](https://github.com/armbrustlab/fsc-size-calibration)) and carbon content ([fsc-poc-calibration](https://github.com/armbrustlab/fsc-poc-calibration)).
3. ```Population data```: Perform aggregate statistics along with error propagation for the different populations.
The cell population identification (```gated``` data) and diameter and carbon content (```calibrated``` data) of each OPP are saved as separate text files with a similar file structure as the RAW data. The metadata, gating scheme, and aggregated statistics for each step are saved to a SQL database using SQLite3.

### Install
In this project's directory, run the install script `setup.R`. This will install `devtools`, `BiocManager`, and any dependencies listed in `DESCRIPTION`. It will not upgrade any packages. To upgrade dependencies, manually run the install commands in `setup.R` in an interactive R environment making sure to set install options for upgrading or updating appropriately.

```
Rscript setup.R
```

### Analysis
To get started with the analysis, go to the [wiki](https://github.com/armbrustlab/popcycle/wiki/SeaFlow-data-analysis-tutorial)
