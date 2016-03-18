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
    return(area.r2 - area.r1)
  }
  cen.ang1 <-  2*acos((.5*cw)/(r1))
  cen.ang2 <-  2*acos((.5*cw)/(r2))
  acs1 <- (r1^2/2)*(cen.ang1-(sin(cen.ang1)))
  acs2 <- (r2^2/2)*(cen.ang2-(sin(cen.ang2)))
  # special case where inner ring is less than core size, but outer
  # ring is larger than core size
  if (2*r1 < cw & 2*r2 > cw) {
    return((area.r2- 2*acs2 -area.r1)/2)
  }
  rarea <- (((area.r2- 2*acs2)- (area.r1- 2*acs1))/2)
  return(rarea)
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

# Obtain data from masters_trees.csv to merge with trees.csv data
trees <- read.csv("../data/masters_trees.csv", stringsAsFactors=FALSE)

# Creates temporary list of all ring files for later merge
ring_files <- list.files("../data/tree_ring_coordinates", full.names=TRUE)

# create a list of coordinate dataframes (1 per tree), concatenate these all
# into one df, then merge (innner_join) with the trees.csv data
ring_data <- rbind_all(lapply(ring_files, read_ring_coord_file)) %>%
    inner_join(trees)

ring_data$ring.area <- NA
for(i in 1:length(ring_data$r1)) {
  ring_data$ring.area[i] <- core.area(BORER_WIDTH, ring_data$r1[i], ring_data$r2[i])
}
# clean up unneeded variables
rm(ring_files)



# Exploring data
ggplot(ring_data, aes(age, resin.duct.count/ring.area, color=mtn)) +
    geom_point() +
    scale_y_log10() +
    facet_grid(spcode ~ .)

# f<- ddply(subset(ring_data, resin.duct.count!="NA"), .(spcode, calendar.year, mtn),
#       summarize, resin.duct.average = (mean(resin.duct.count)/ mean(resin.ducts_area)),
#       n = length(tag))
# 
# ggplot(f, aes(calendar.year, resin.duct.average, color=mtn)) +
#   geom_jitter()+ 
#   facet_grid(spcode ~ .)
# 
# resin.duct.count.lm <- lm(resin.duct.count~ spcode + spcode:mtn + mtn, data=ring_data)
# summary(resin.duct.count.lm)
# anova(resin.duct.count.lm)
