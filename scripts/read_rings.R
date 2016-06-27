# read_rings.R

# Read in tree ring coordinates from individual csv files, calculate ring
# widths and areas. Provides data frame "ring_data" to global namespace. The
# tree ring data files include a row for each ring and a final row giving the
# coordinates of the vascular cambium. So the final row of data is only used to
# calculate the width associated with the outermost ring (penultimate row of
# data). Also provides "trees" data frame which uses values from raster_data.R
# to add to data frame.  Make sure raster_data.R is run once to produce .rds 
# files which will be read here.

# What is produced:
# -"ring_data"" data frame is added to the global namespace
# -"trees" data frame is added to the global namespace.  Values obtained
#   from raster files are added as well from raster_data.R.
# -"trees.sum" data frame is added to the global namespace
# -"sdist" and "get_widths" function which calculates distance between rings
# -"core.area" function that calculates the area in between each tree ring,
#   accounts for special cases as well.
# -"bai" function calculates basal area increment
# -"read_ring_coord_file" function reads in ring data csv files and
#   calculates variables: calendar year, age, ring.dist and ring.width.
# -Some exploratory analyses

# Sources script that populates yearly precipitation values as well as
# loading relevant packages for analyses.
source("./read_precip_drought_data.R")


SAMPLE_YEAR <- 2015 # year trees were cored, so last full ring will be this one
                    # assuming that sampling occured after enough growth to
                    # distinguish. The code should probably actually check the
                    # sample date in trees.csv, but this is ok for now if all
                    # were sampled in mid-late summer 2015.

BORER_WIDTH = 1.2

sdist <- function(x1,y1,x2,y2) {
    sqrt( (x1-x2)^2 + (y1-y2)^2)
}

# Obtain the distance between rings. Coordinates are associated with the inner
# boundary of a ring, so we obtain values for rows n:(n-1).
get_widths <- function(distances) {
    c(distances[2:length(distances)], NA) - distances
}

# Calculates the ring area enclosed between each tree ring using core
# width, inner radius, and outer radius.  If statements are in place
# for special case scenarios where radii are less than core width.
# Calculations assume a perfect circle for each radius and subtracts
# the area of the circle outside of the core.
core.area <- function(cw, r1, r2) {
  area.r1<- (pi*r1^2)
  area.r2<- (pi*r2^2)
  # special case if both rings are less than core size.
  if(2*r2 < cw ) {
    return((area.r2 - area.r1)/2)
  }
  cen.ang2 <-  2*acos((.5*cw)/(r2))
  acs2 <- (r2^2/2)*(cen.ang2-(sin(cen.ang2)))
  # special case where inner ring is less than core size, but outer
  # ring is larger than core size
  if (2*r1 < cw & 2*r2 > cw) {
    return((area.r2- 2*acs2 -area.r1)/2)
  }
  cen.ang1 <-  2*acos((.5*cw)/(r1))
  acs1 <- (r1^2/2)*(cen.ang1-(sin(cen.ang1)))
  rarea <- (((area.r2- 2*acs2)- (area.r1- 2*acs1))/2)
  return(rarea)
}

# Simple function to calculate basal area increment
bai <- function(r1, r2) {
  area.r1<- (pi*r1^2)
  area.r2<- (pi*r2^2)
  barea <- (area.r2- area.r1)
}

# Reads a ring coordinate file and returns a data frame with several new
# calculated columns: calendar year, age, ring.dist and ring.width
read_ring_coord_file <- function(filename) {
    df <- read.csv(filename, stringsAsFactors=FALSE)
    names(df) <- c("ring", "x", "y", "resin.duct.count")
    df$tag <- strsplit(basename(filename), "\\.")[[1]][1]
    x1 <- df$x[1]
    y1 <- df$y[1]
    df <- df %>% mutate(calendar.year=SAMPLE_YEAR + ring - max(ring) + 1,
                        ring.age=max(ring)-ring,
                        # Convert coordinate pixels from inches to cm
                        r1=sdist(x1, y1, x, y)*2.54,
                        ring.width=get_widths(r1),
                        r2 = r1+ring.width)
    return(subset(df, !is.na(ring.width)) ) # throw away last row in each df
}

# Obtain data from masters_trees.csv to merge with trees.csv data
trees <- read.csv("../data/masters_trees.csv", stringsAsFactors=FALSE)

# Read .rds files created from raster_data.R script for each mtn range  
cm_raster_data<- readRDS(file="../results/cm_raster_data.rds")
dm_raster_data<- readRDS(file="../results/dm_raster_data.rds")
gm_raster_data<- readRDS(file="../results/gm_raster_data.rds")

# Create new data frame with raster info on all trees as well as other 
# variables measured and calculated
trees <- trees %>% mutate(gps.elev=elev) %>% dplyr::select(-lat, -lon, -elev) %>%
  inner_join(bind_rows(cm_raster_data, dm_raster_data, gm_raster_data))

# Creates temporary list of all ring files for later merge
ring_files <- list.files("../data/tree_ring_coordinates", full.names=TRUE)

# create a list of coordinate dataframes (1 per tree), concatenate these all
# into one df, then merge (innner_join) with the trees.csv data
ring_data <- bind_rows(lapply(ring_files, read_ring_coord_file)) %>%
    inner_join(trees)

# Precipitation data in it's current state cannot be used due to the
# inconsistencies involved with availability and actual values calculated.
# Code is still included in the script in case we decide to use it later.

# # join precipitation values from dataframe created with precip_data.R into
# # previously created dataframe and rename it to ring_data.
# # ring_data <-left_join(ring_data_first, yearly_precip_data, by= c("mtn","calendar.year"))
# 
# ring_first <-left_join(ring_data, yearly_precip_data, by= c("mtn","calendar.year"))
# 
# # Calculates distance from each tree to the corresponding sensor
# # in the mountain range.
#  ring_first$sensor_dist <- NA
# # lon.x and lat.x are coordinates from the tree, lon.y and lat.y are
# # coordinates from the precipitation sensor
# for(i in 1:nrow(ring_first)) {
#    ring_first$sensor_dist[i] <- gcd.hf(ring_first$lon.x[i], ring_first$lat.x[i], ring_first$lon.y[i], ring_first$lat.y[i])
# }
# 
# # lat approach
# # tree coords are x, station coords are y
# ## ring_first <- mutate(ring_first, sensor_dist =  gcd.hf(ring_first$lon.x,
# ##                                                        ring_first$lat.x,
# ##                                                        ring_first$lon.y,
# ##                                                        ring_first$lat.y))
# 
#  
# # temporary dataframe for tree core years that had values for precipitation
# # based on station location
# temp_df <- ring_first %>% group_by(tag, ring) %>% slice(which.min(sensor_dist))
# # temporary dataframe for tree core years that did not have a value for
# # precipitation based on station location
# temp_df2 <- filter(ring_first, is.na(ring_first$sensor_dist))
#  
# # Combine temporary data frames together and arrange them
# ring_data <- bind_rows(temp_df, temp_df2) %>% arrange(tag, ring)

# Add drought values to the data frame.
ring_data<- left_join(ring_data, yearly_drought, by= "calendar.year")

# Calculate ring area and assign value for each year
ring_data$ring.area <- NA
for(i in 1:length(ring_data$r1)) {
  ring_data$ring.area[i] <- core.area(BORER_WIDTH, ring_data$r1[i], ring_data$r2[i])
}

# Calculates new column for basal area index for each year
ring_data$bai <- NA
for(i in 1:length(ring_data$r1)) {
  ring_data$bai[i] <- bai(ring_data$r1[i], ring_data$r2[i])
}

# Creates new column for resin duct density at each year
ring_data <- mutate(ring_data, duct.density=resin.duct.count/ring.area)

ring_data <- mutate(ring_data, total.duct.count= resin.duct.count*(bai/ring.area))

# Remove some columns that don't pertain to analyses, but are in the
# original data set relegated to notes.
cols.dont.want <- c("x", "y", "core.taken", "pith",
                    "needles.collected", "condition", "barkbeetle.attack",
                    "trail.area", "note", "lat.y", "lon.y")
ring_data <- ring_data[, ! names(ring_data) %in% cols.dont.want, drop = FALSE]

# clean up unneeded variables
rm(ring_files, cm_raster_data,dm_raster_data, gm_raster_data, cols.dont.want)
   # ring_first, temp_df, temp_df2)

# rename rings column to age
ring_data <- rename(ring_data, age = ring)

# Calculate summaries per tree
trees.sum <- ring_data %>% group_by(tag) %>%
  dplyr::summarize(avg.age = mean(age),
            age.sd = sd(age),
            age.min = min(age), max.age = max(age),
            duct.count.mean = mean(resin.duct.count, na.rm = TRUE),
            duct.count.sd = sd(resin.duct.count, na.rm = TRUE),
            duct.den.mean = mean(duct.density, na.rm = TRUE),
            duct.den.sd = sd(duct.density, na.rm = TRUE),
            total.duct.count.mean = mean(total.duct.count, na.rm = TRUE),
            total.duct.count.sd = sd(total.duct.count, na.rm = TRUE),
            ring.width.mean = mean(ring.width),
            ring.width.sd = sd(ring.width),
            bai.mean = mean(bai),
            bai.sd = sd(bai),
            PMDI.mean = mean(PMDI_3yrlag, na.rm = TRUE),
            PMDI.sd = sd(PMDI_3yrlag, na.rm = TRUE)
            ) %>% inner_join(trees)


# Exploring data

## # Make sure graph-themes.R is loaded, but if not:
## source("./graph-themes.R")

## # Age vs. resin duct density
## ggplot(trees.sum, aes(max.age, duct.den.mean, color=mtn)) +
##     geom_point() +
##     scale_y_log10() +
##     facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
##     theme(strip.text.y = element_text(size=5)) +
##     labs(x= "age",
##          y= "resin duct density")+
##     scale_color_manual(name= "Mountain range",
##                        labels = mountain_names,
##                        values= mycolours)

