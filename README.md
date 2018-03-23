pine-resin-duct
===============

This repo contains data, code, figures, and methods for the Schwilk Lab resin duct project that originated with Erik Lindberg's MS thesis on resin duct and growth characteristics of pine trees in west Texas mountains. See [ms-tables-figs.R](https://github.com/schwilklab/pine-resin-duct/blob/master/scripts/ms-tables-figs.R) for a single script that will produce all analyses, tables and figures as submitted to AJB (authors: Lindberg, Ferrenberg, and Schwilk).

DOI for this repository:

[![DOI](https://zenodo.org/badge/33746972.svg)](https://zenodo.org/badge/latestdoi/33746972)

## Requirements ##

### R and R packages ###

These analyses require R and the following packages: dplyr, stringr, ggplot2, rgdal, raster, lme4, afex, ggmap, maptools. All packages are available directly through R.


## Specific Methods ##

### Topographic variable calculations ###

See [topo_grid methods documentation](https://github.com/schwilklab/skyisland-climate/blob/master/methods/topo_grid_methods.md) located in the skyisland-climate repo hosted by schwilklab.

### Drought value calculations ###

Palmers Modified Drought Index (PMDI) values were obtained from [NOAA website](http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp#) and saved as a .csv file locally.  Further explanation regarding the drought values can be found [here](http://www.ncdc.noaa.gov/temp-and-precip/drought/historical-palmers/overview).  Drought values refer to the trans-Pecos region of Texas, which exact boundaries of the spatial location in reference can be found [here](http://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=indices&layers=01&node=gis).  Code then calculates a three year running average of drought values and assigns it to a dataframe which will be called upon in a different script.

### Statistical models, tables and figures for manuscript ###

The script [ms-tables-figs.R](https://github.com/schwilklab/skyisland-climate/blob/master/scripts/ms-tables) conducts the necessary final analysis for the manuscript as submitted the AJB.  Some intermediate steps in deciding final model structure are not shown in this code but are descrbed in the manuscript). 

This script first sources a script to read data and run transformations (`read_all.R`). Then this script fits lmer-models and calculates approximate p-values, produces html and odt tables (tables 1, 2 , S1 and S2), and produces all figures for the submitted ms (Figs1-5).

