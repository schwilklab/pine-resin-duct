# read_raster_data.R
 
# This takes .asc files from topo_grids (download seperately), and
# creates a raster stack for each mountain range.  Temporary dataframes
# are produced with each tree's raster value for each raster file based
# on latitude and longitude. Values are then merged with the output of
# ring_data to create the final data frame with all relevant information
# to conduct analyses. This only needs to be run once to produce the 
# raster stack data, and only needs to be run again if the .asc files
# change, since all data is stored as a .rds file in the scripts.

# What is produced:
# -Three functions used to create raster files and extract relevant
#  raster information for each tree location
# -Raster stack files for each mountain range
# -Raster values for all tagged trees that reside within raster data
# -Function to create a map using ggmap
# -Exploratory analyses to look at data


library(rgdal)
library(raster)
library(ggmap)
library(dplyr)

PROJ_STRING <- "+proj=longlat +ellps=WGS84 +datum=WGS84"

readGrid <- function(filename) {
    colname <- sub("[.][^.]*$", "", basename(filename))
    grid <- maptools::readAsciiGrid(filename, colname=colname,
                                    proj4string=sp::CRS(PROJ_STRING))
    return(raster::raster(grid))
}

readGridFolder <- function(fpath) {
    ## get list of grid files
    ascii_grids <- list.files(path=fpath, pattern = "*.asc", full.names=TRUE)
    ## Use filenames without extensions as column names
    layers <- sapply(ascii_grids, readGrid)
    names(layers) <- sapply(layers, names) # get colnames for list item names
    # stack layers
    topostack <- raster::stack(layers)
    return(topostack)
}

extractVals1Mtn <- function(themtn, topostack) {
    trees.mtn <- trees %>% filter(mtn==themtn & core.taken=="Y" & pith=="Y") %>%
        dplyr::select(tag, lon, lat)
    coords <- trees.mtn %>% dplyr::select(lon, lat) %>%
        SpatialPoints(proj4string=sp::CRS(PROJ_STRING))
    return(cbind(trees.mtn, raster::extract(topostack, coords)))
}



# create raster stacks of topo variables
# this takes a few minutes! maybe cache?
cm_raster_stack <- readGridFolder("../topo_grids/CM")
dm_raster_stack <- readGridFolder("../topo_grids/DM")
gm_raster_stack <- readGridFolder("../topo_grids/GM")

# Save raster stack files as .rds files to minimize time to load
saveRDS(cm_raster_stack, file = "../results/cm_raster_stack.rds")
saveRDS(dm_raster_stack, file = "../results/dm_raster_stack.rds")
saveRDS(gm_raster_stack, file = "../results/gm_raster_stack.rds")


# now subset those topo variables to just our tree locations
cm_raster_data <- extractVals1Mtn("CM", cm_raster_stack)
dm_raster_data <- extractVals1Mtn("DM", dm_raster_stack)
gm_raster_data <- extractVals1Mtn("GM", gm_raster_stack)

# Save information as an RDS file for easy upload later on since loading
# raster values takes a long time
saveRDS(cm_raster_data, file = "../results/cm_raster_data.rds")
saveRDS(dm_raster_data, file = "../results/dm_raster_data.rds")
saveRDS(gm_raster_data, file = "../results/gm_raster_data.rds")




################################################################
## Visualization code below

# explore these data

cm_topo <- data.frame(rasterToPoints(cm_raster_stack))
ggplot(cm_topo, aes(x=slope, y=radiation, color=elev)) +
    geom_point()

dm_topo <- data.frame(rasterToPoints(dm_raster_stack))
ggplot(dm_topo, aes(x=slope, y=radiation, color=elev)) +
    geom_point()

# Crap, radiation is useless. Need to check with Helen.

## Maps

# Function used to create map extents
get_gmap <- function(df) {
    return(get_map(location = c(left=min(df$lon-0.015),
                                bottom=min(df$lat-0.015),
                                right=max(df$lon+0.015),
                                top=max(df$lat+0.015))))
    }


newmap <- get_gmap(cm_raster_data)
cm.map <- ggmap(newmap) + geom_point(aes(x=lon, y=lat, size=DBH, color=elev), data=trees)
cm.map


ggplot(subset(ring_data_all, spcode = "PIPO"), aes(x= radiation, y = resin.duct.count/ring.area)) +
    facet_grid(. ~ mtn) + geom_point()

