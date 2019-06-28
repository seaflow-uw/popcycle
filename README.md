SeaFlow project
========
<img src="https://github.com/armbrustlab/seaflow-sfl/blob/master/cruise-track.png" alt="SeaFlow cruisetracks"	title="SeaFlow cruisetracks" align="left" style="float" width="400">

[SeaFlow](https://armbrustlab.ocean.washington.edu/tools/seaflow/) is a new environmental flow cytometer designed by the [Armbrust lab](https://armbrustlab.ocean.washington.edu) to be deployed on oceanographic research vessels to continuously monitor photosynthetic microorganisms. Since its first deployment in 2008, the [SeaFlow](https://armbrustlab.ocean.washington.edu/tools/seaflow/) instrument has collected over 200,000 samples in surface waters of the North Pacific and Atlantic Oceans. The geographic distribution of marine phytoplankton, their optical characteristics (size and pigment content), and their dynamics in relation to environmental factors are of major interest to oceanographers. 

  Unlike a conventional flow cytometer, [SeaFlow](https://armbrustlab.ocean.washington.edu/tools/seaflow/) directly analyzes a raw stream of seawater using two detectors that determine the position of the particle within the focal region of the instrument optical system ([Swalwell et al. 2011](https://doi.org/10.4319/lom.2011.9.466)). With this technology, measurements from particles that pass through the ideal focal position of the collection optics can be differentiated from improperly positioned particles, producing a measurement equivalent to that obtained with a conventional cytometer (see [OPP filtration](https://github.com/armbrustlab/seaflow-filter)). The ratio of these optimally positioned particles (OPP) to the total detectable particles is used to retrieve the volumetric flow rate, allowing accurate estimation of cell abundances (see [Virtual Core calibration](https://github.com/armbrustlab/seaflow-virtualcore)). Each particle is defined by its light scatter and by two different wavelengths of fluorescence associated with pigments:  chlorophyll (690 nm for red fluorescence) and phycoerythrin (570 nm for orange fluorescence). These parameters allow the discrimination between cells and detritus or suspended sediments and between photosynthetic and non-photosynthetic organisms. 

Popcycle
========
**Popcycle** is an R package that uses a database management system approach to facilitate the analysis of [SeaFlow](https://armbrustlab.ocean.washington.edu/tools/seaflow/) data. 

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
