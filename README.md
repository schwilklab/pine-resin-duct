bark-beetle
===========

This repo contains data, code, figures, and methods for Erik Lindberg's MS thesis on resin duct and growth characteristics of pine trees in west Texas mountains.


## Requirements ##

### R and R packages ###

These analyses require R and the following packages: dplyr, stringr, ggplot2, rgdal, raster, lme4, MuMIn, afex, ggmap, maptools, brms, shinystan, and shiny. All packages are available directly through R.


## Specific Methods ##

### Topographic variable calculations ###

See [topo_grid methods documentation](https://github.com/schwilklab/skyisland-climate/blob/master/methods/topo_grid_methods.md) located in the skyisland-climate repo hosted by schwilklab.  Documentation provided by Dr. Helen Poulos and Dr. Dylan Schwilk.

### Drought value calculations ###

Palmers Modified Drought Index (PMDI) values were obtained from [NOAA website](http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp#) and saved as a .csv file locally.  Further explanation regarding the drought values can be found [here](http://www.ncdc.noaa.gov/temp-and-precip/drought/historical-palmers/overview).  Drought values refer to the trans-Pecos region of Texas, which exact boundaries of the spatial location in reference can be found [here](http://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=indices&layers=01&node=gis).  Code then calculates a three year running average of drought values and assigns it to a dataframe which will be called upon in a different script.

### Lmer models ###

When conducting analyses, use [lmer_mods.R](./scripts/lmer_mods.R).  This script will call upon read_rings.R, which calls upon read_precip_drought_data.R and read_raster_data.R to create a dataframe containing all corresponding data that was obtained.  Detailed specifics regarding how everything is calculated is commented out in the R scripts.
