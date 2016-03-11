# rings.R

# Read in tree ring coordinates from individual csv files, calculate ring
# widths and areas. Provides data frame "ring_data" to global namespace. The
# tree ring data files include a row for each ring and a final row giving the
# coordinates of the vascular cambium. So the final row of data is only used to
# calculate the width associated with the outermost ring (penultimate row of
# data).

library(dplyr)
library(ggplot2)

SAMPLE_YEAR <- 2015 # year trees were cored, so last full ring will be this one
                    # assuming that sampling occured after enough growth to
                    # distinguish. The code should probably actually check the
                    # sample date in trees.csv, but this is ok for now if all
                    # were sampled in mid-late summer 2015.

BORER_WIDTH = 1.2

sdist <- function(x1,y1,x2,y2) {
    sqrt( (x1-x2)^2 + (y1-y2)^2)
}

# Shifts the values in a single column by amount specified
shift<- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}


# Obtain the distance between rings. Coordinates are associated with the inner
# boundary of a ring, so we obtain values for rows n:(n-1).
get_widths <- function(distances) {
    c(distances[2:length(distances)], NA) - distances
}

# Calculates width of the core based on radius. There are a few rings that
# have a smaller radius than the core width, so this is necessary for
# accurate ring area calculations.
core.widths <- function (radius2, core.width = BORER_WIDTH) {
  diameter <- 2 * radius2
  if (diameter > core.width) {
    diameter = core.width
  }
  return(diameter)
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
                        age=max(ring)-ring,
                        # Convert coordinate pixels from inches to cm
                        r1=sdist(x1, y1, x, y)*2.54,
                        ring.width=get_widths(r1),
                        r2 = r1+ring.width)

    return(subset(df, !is.na(ring.width)) ) # throw away last row in each df
}

# needed for tree age:
trees <- read.csv("../data/masters_trees.csv", stringsAsFactors=FALSE)
## DWS: How tree age?

ring_files <- list.files("../data/tree_ring_coordinates", full.names=TRUE)

# create a list of coordinate dataframes (1 per tree), concatenate these all
# into one df, then merge (innner_join) with the trees.csv data
ring_data <- rbind_all(lapply(ring_files, read_ring_coord_file)) %>%
    inner_join(trees)

# clean up unneeded variables
rm(ring_files)

# Exploring data
ggplot(ring_data, aes(age, resin.duct.count/ring.width, color=mtn)) +
    geom_point() +
    scale_y_log10() +
    facet_grid(spcode ~ .)

## Code for calculating area ring.area.  There's an error in my 
## function that I wrote for core.widths.  I think I am close to 
## calculating area, but I don't think my code is very effiicent.
## Will continue to work on this.





tag.list <- unique(ring_data$tag)
ring.area <- data.frame (tag = ring_data$tag, calendar.year= ring_data$calendar.year,
                         area = rep(NA, length(ring_data$tag)))


for (i in length(tag.list)) {
  core <- ring_data[ring_data$tag == tag.list[i], ]
  
  for (j in length(core$radius)){
    r1 <- core$ring.dist
    r2 <- core$radius
    cw1 <- core.widths(r1)
    cw2 <- core.widths(r2)
    core.width <-cw2
    cen.ang1 <-  2*asin(cw1/(2*r1))
    cen.ang2 <-  2*asin(cw2/(2*r2))
    acs1 <- (r1^2/2)*(cen.ang1-(sin(cen.ang1)))
    acs2 <- (r2^2/2)*(cen.ang2-(sin(cen.ang2)))
    ring.area$area[ring.area$tag == tag.list[i], i ] <- core$ring.width(j)*
      core.width-acs1+acs2
  }
}

core.area <- function(C, r1, r2) {

    # special case for ring less than core size:
    if(2*r2 < C) {
        return((2*pi*r2^2 - 2*pi*r1^2)/2)
    }
    
    
    cen.ang1 <-  2*asin(C/(2*r1))
    cen.ang2 <-  2*asin(C/(2*r2))
    # special case
    if (r1 < C) {
       # return 
         C * (r2-r1) - (2*pi*r1^2)/2  + acs2
    } else {
        acs1 <- (r1^2/2)*(cen.ang1-(sin(cen.ang1)))
    }
    
    acs2 <- (r2^2/2)*(cen.ang2-(sin(cen.ang2)))
    rarea <- C * (r2-r1) -acs1 + acs2
    return(rarea)
}

#ring_data <- ring_data %>% mutate(ring.area = core.area(BORER_WIDTH, r1, r2))


ring_data$ring.area <- NA
for(i in 1:length(ring_data$r1)) {
    ring_data$ring.area[i] <- core.area(BORER_WIDTH, ring_data$r1[i], ring_data$r2[i])
}

    


# Testing to make sure equation works. This can be deleted once I am
# able to run the actual code.

# Example for calculating core are using one tree ring's values

#r1<- 1.31184545 # inner radius (ring.dist)
#r2<- 1.33662176 # outer radius
#cw1<- core.widths(r1) # core width radius 1
#cw2<- core.widths(r2) # core width radius 2
#core.width<- cw2 # core width for area calculation
#cen.ang2<- 2*asin(cw2/(2*r2)) #radians
#cen.ang1<- 2*asin(cw1/(2*r1)) #radians
#ring.width<- .02477631 # ring width
#acs1<- (r1^2/2)*(cen.ang1-(sin(cen.ang1))) #area under curve 1
#acs2<- (r2^2/2)*(cen.ang2-(sin(cen.ang2))) #area under curve 2
#ring.area<- ring.width*core.width -acs1+acs2
#ring.area.simple<- ring.width*core.width # no under curve area
