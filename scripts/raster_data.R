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


library(rgdal)
library(raster)
library(ggmap)
library(dplyr)

source("./data-checks.R")
source("./rings.R")

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



# now subset those topo variables to just our tree locations
cm_raster_data <- extractVals1Mtn("CM", cm_raster_stack)
dm_raster_data <- extractVals1Mtn("DM", dm_raster_stack)
gm_raster_data <- extractVals1Mtn("GM", gm_raster_stack)

# a nice big data frame with a row for each tree with all tree-specific data
trees <- trees %>% mutate(gps.elev=elev) %>% dplyr::select(-lat, -lon, -elev) %>%
    inner_join(bind_rows(cm_raster_data, dm_raster_data, gm_raster_data))


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

get_gmap <- function(df) {
    return(get_map(location = c(left=min(df$lon-0.015),
                                bottom=min(df$lat-0.015),
                                right=max(df$lon+0.015),
                                top=max(df$lat+0.015))))
}

newmap <- get_gmap(cm_raster_data)
cm.map <- ggmap(newmap) + geom_point(aes(x=lon, y=lat, size=DBH, color=elev), data=trees)
cm.map



# merge information into ring_data, creating a new dataframe. not sure we need
# to do this. You might summarize by tree first?
ring_data_all <- left_join(ring_data, trees, by = "tag") # duplicated info


ggplot(subset(ring_data_all, spcode = "PIPO"), aes(x= radiation, y = resin.duct.count/ring.area)) +
    facet_grid(. ~ mtn) + geom_point()

# remove temporary dataframes and files created
rm(cm_raster_data, dm_raster_data, gm_raster_data)
