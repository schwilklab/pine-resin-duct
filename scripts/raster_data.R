# raster_data.R
# 
# This takes .asc files from topo_grids (download seperately), and
# creates a raster stack for each mountain range.  Temporary dataframes
# are produced with each tree's raster value for each raster file based
# on latitude and longitude. Values are then merged with the output of
# ring_data to create the final data frame with all relevant information
# to conduct analyses.  Information from here can most likely be merged
# into rings.R later on, but until this is perfected, I will keep as a 
# seperate R file.

library(dplyr)
library(rgdal)
library(raster)

source("./data-checks.R")
source("./rings.R")

# Create dataframes containing tree coordinates in each mountain range
cm.coordinates<- trees %>% filter(mtn=="CM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(lat, lon)
dm.coordinates<- trees %>% filter(mtn=="DM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(lat, lon)
gm.coordinates<- trees %>% filter(mtn=="GM" & core.taken=="Y" & pith=="Y") %>%
  dplyr::select(lat, lon)

# Now to create a raster stack of the .asc files

## Chisos mountains
## make a list of file names:  
cm_asc_list<- list.files(path= "../topo_grids/cm", pattern = "*.asc", full.names=TRUE)
## removing the last two files in list becaues they are not .asc files
cm_asc_list<- cm_asc_list[c(-17, -18)]
## turn these into a list of RasterLayer objects  
cm_raster_stack <- stack(cm_asc_list)

# Do this for the remaining mountain ranges:

## Davis mountains
dm_asc_list<- list.files("../topo_grids/dm", pattern = "*.asc", full.names=TRUE)
dm_raster_stack <- stack(dm_asc_list)

## Guadalupe mountains

### When I run this, is get an error: 
### "Error in compareRaster(rasters) : different extent" This only 
gm_asc_list<- list.files("../topo_grids/gm", pattern = "*.asc", full.names=TRUE)
# ldist_ridge2.asc seems to be the culprit since it's extent is different
# than the rest of the .asc files, but when I remove it, I still get the
# same error when I try to stack the rasters.  I have manually checked
# each .asc file's extent when converted to a raster layer individually
# and all of them have the same extent, except ldist_ridge2.asc. I can
# redefine the extent for all of them, but ldist_ridge2.asc is very 
# different from the rest of them which would make me lose a lot of
# information.

# Extent for ldist_ridge2.asc
# class       : Extent 
# xmin        : -104.5746 
# xmax        : -103.6716 
# ymin        : 30.4486 
# ymax        : 31.07305 

# Extent for all other gm .asc files
# class       : Extent 
# xmin        : -105.1699 
# xmax        : -104.4852 
# ymin        : 31.71823 
# ymax        : 32.18656 


#gm_asc_list<- gm_asc_list[c(-6)]
#gm_raster_stack <- stack(gm_asc_list)


### Update, when I run this now, I am able to populate a new dataframe,
### but I only extract NA values from the raster file I create.  Upon
### further checking, it seems that the raster file that I created
### does have values associated with it, but I am not extracting any of
### the values with my coordinates.
cm_raster_data <- data.frame(cm.coordinates,
                           raster::extract(cm_raster_stack,cm.coordinates))
dm_raster_data <- data.frame(dm.coordinates,
                           raster::extract(dm_raster_stack,dm.coordinates))
#gm_raster_data <- data.frame(gm.coordinates,
#                           raster::extract(gm_raster_stack,cm.coordinates))

### Update: when I run this now, I am able to populate a new dataframe,
### but I only extract NA values from the raster file I create.  Upon
### further checking, it seems that the raster file that I created
### does have values associated with it, but it is possible that there 
### is not available information at those data points.

# combine all raster data frames into one
all_raster_data<- bind_rows(cm_raster_data, dm_raster_data)#,gm_raster_data)
# merge information into ring_data, creating a new dataframe.
ring_data_all<- left_join(ring_data, all_raster_data, by = c("lat", "lon"))

# remove temporary dataframes and files created
rm(cm.coordinates,dm.coordinates, gm.coordinates, cm_asc_list,
   dm_asc_list, gm_asc_list, cm_raster_data, dm_raster_data,
   all_raster_data)
