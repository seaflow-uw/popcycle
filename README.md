Popcycle
========
**Popcycle** is an R package that offers a reproducible approach to process, calibrate and curate flow cytometry data collected by SeaFlow.

<img src="documentation/images/seaflow-workflow.png?raw=true" alt="Popcycle workflow"
	title="Popcycle workflow" align="right" style="float" width="500">
Raw  data are stored every 3 minutes in a custom binary file format (RAW data) consisting of six to eight 16-bit integer channels, along with [metadata](https://github.com/seaflow-uw/seaflow-sfl) provided by the ship's data system (e.g., time, location, sea surface temperature, salinity, light intensity). The files are stored in day-of-year-labeled directories, each containing raw files with the associated log file.

The software package performs 4 key analyses:
1. ```Filtering```: Filter raw particle data down to optimally posistioned particles (OPP).
2. ```Gating```: Classification of phytoplankton cell populations using a mixture of manual gating and a semi-supervized clusterting algorithm.
3. ```Light scatter conversion```: Convert light scattering of each particle to cell diameter ([fsc-size-calibration](https://github.com/seaflow-uw/fsc-size-calibration)) and carbon content ([fsc-poc-calibration](https://github.com/seaflow-uw/fsc-poc-calibration)).
4. ```Population data```: Perform aggregate statistics along with error propagation for the different populations.

Filtered OPP data are stored in hourly [Parquet](https://github.com/apache/parquet-format) files.
Gated data (cell population identification) and calibrated data (diameter and carbon content) for each hourly OPP file are saved in separate hourly VCT Parquet files. The metadata, filtering and gating parameters, and aggregated statistics for each step are saved to a SQLite3 database files.

### Analysis
To get started with the analysis, go to the [wiki](https://github.com/seaflow-uw/popcycle/wiki/SeaFlow-data-analysis-tutorial)
