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

# -"mdata" data frame is added to the global namespace and is used as the
#   dataframe for models.

# -"ring_data"" data frame is added to the global namespace
# -"sdist" and "get_widths" function which calculates distance between rings
# -"core.area" function that calculates the area in between each tree ring,
#   accounts for special cases as well.
# -"bai" function calculates basal area increment
# -"read_ring_coord_file" function reads in ring data csv files and
#   calculates variables: calendar year, age, ring.dist and ring.width.


library(dplyr)

###############################################################################
## Constants
###############################################################################

SAMPLE_YEAR <- 2015 # year trees were cored, so last full ring will be this one
                    # assuming that sampling occured after enough growth to
                    # distinguish. The code should probably actually check the
                    # sample date in trees.csv, but this is ok for now if all
                    # were sampled in mid-late summer 2015.

BORER_WIDTH = 1.2


###############################################################################
## Functions
###############################################################################

sdist <- function(x1,y1,x2,y2) {
    sqrt( (x1-x2)^2 + (y1-y2)^2)
}

# Obtain the distance between rings. Coordinates are associated with the inner
# boundary of a ring, so we obtain values for rows n:(n-1).
get_widths <- function(distances) {
    c(distances[2:length(distances)], NA) - distances
}

# Calculates the ring area enclosed between each tree ring using core width,
# inner radius, and outer radius. If statements are in place for special case
# scenarios where radii are less than core width. Calculations assume a perfect
# circle for each radius and subtracts the area of the circle outside of the
# core.
core.area <- function(r1, r2, cw = BORER_WIDTH) {
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
  barea <- (area.r2 - area.r1)
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


###############################################################################
## Main script
###############################################################################

# Creates temporary list of all ring files for later merge
ring_files <- list.files("../data/tree_ring_coordinates", full.names=TRUE)

# create a list of coordinate dataframes (1 per tree), concatenate these all
# into one df, merge with data on each tagged tree
ring_data <- bind_rows(lapply(ring_files, read_ring_coord_file)) %>%
    inner_join(trees)

# rename rings column to age
ring_data <- rename(ring_data, age = ring)

# Obtain species and subsection info. We need this because we do regional curve
# standardization by subsection.
ring_data <- left_join(ring_data, species, by = "spcode")

# Calculate ring area, BAI and resin duct density
ring_data <- ring_data %>% rowwise() %>% mutate(ring.area = core.area(r1, r2),
                                                bai = bai(r1, r2),
                                                duct.density = resin.duct.count/ring.area,
                                                total.duct.count = resin.duct.count*(bai/ring.area))
                    
# Remove some columns that don't pertain to analyses, but are in the
# original data set relegated to notes.

ring_data <- ring_data %>% ungroup() %>% select(-x, -y, -core.taken, -pith,
                                                -needles.collected, -condition,
                                                -barkbeetle.attack, -trail.area, -note)


# Detrended ring widths using regional curve standardization (per subsection).
# The standard dendro way is to DIVIDE the actual rw by the predicted (where
# predicted is based on polynomial or spline). But this leads to bias and using
# residuals is clearly a better method. See Cook and Peters 1997:
# http://journals.sagepub.com/doi/abs/10.1177/095968369700700314
RCS <- function(rw, age) {
  df <- data.frame(rw=rw, age=age)
  mod <- lm(rw ~ poly(age, 4), data=df) # 4 degree polynomial seems to work. Should justify
  #pred <- predict(mod)
  #r <- rw / pred
  ## mod <- smooth.spline(age, rw, spar=0.67)
  r <- residuals(mod)
  return(r)
}

ring_data <- ring_data %>% group_by(subsection) %>%
  mutate(ring_width_detrended = RCS(ring.width, ring.age)) %>%
  ungroup()



# clean up unneeded variables
rm(ring_files)


