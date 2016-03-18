library(plyr)
library(dplyr)
library(adehabitat)
library(rgdal)
library(raster)

trees <- read.csv("../data/masters_trees.csv")

# Create dataframes containing tree coordinates in each mountain range
cm.coordinates<- trees %>% filter(mtn=="CM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(lat, lon)
dm.coordinates<- trees %>% filter(mtn=="DM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(lat, lon)
gm.coordinates<- trees %>% filter(mtn=="GM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(lat, lon)

# Example data for chisos mountains
cm.elev.asc = import.asc("../topo_grids/CM/elev.asc")
# Convert .asc file to raster
cm.elev.raster= raster::raster(cm.elev.asc)

#### Error occurs when I try to run this next line of code ####
# Obtaining an error when I attempt to use extract:
# Error in (function (classes, fdef, mtable)  : 
# "unable to find an inherited method for function 'extract' for
# signature '"asc", "data.frame"
cm_elev_data <- data.frame(cm.coordinates,
                           raster::extract(cm.elev.raster,cm.coordinates))

### Update, when I run this now, I am able to populate a new dataframe,
### but I only extract NA values from the raster file I create.  Upon
### further checking, it seems that the raster file that I created
### does have values associated with it, but it is possible that there 
### is not available information at those data points.





### Working on obtaining all of the information through a loop.

# Chisos mountains
## make a list of file names:  
#cm_asc_list<- list.files(path= "../topo_grids/cm", pattern = "*.asc", full.names=TRUE)
## removing the last two files in list becaues they are not .asc files
#cm_asc_list<- cm_asc_list[-17]
#cm_asc_list<- cm_asc_list[-17]
## turn these into a list of RasterLayer objects  
#cm_raster_list <- lapply(cm_asc_list, raster)

## Davis mountains
# dm_asc_list<- list.files("../topo_grids/dm", pattern = "*.asc", full.names=TRUE)
# dm_raster_list <- lapply(dm_asc_list, raster)

## Guadalupe mountains
# gm_asc_list<- list.files("../topo_grids/gm", pattern = "*.asc", full.names=TRUE)
# gm_raster_list <- lapply(gm_asc_list, raster)