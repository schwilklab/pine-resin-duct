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
                    # distinguish. The code should porbably actually check the
                    # sample date in trees.csv, but this is ok for now if all
                    # were sampled in mid-late summer 2015.

sdist <- function(x1,y1,x2,y2) {
    sqrt( (x1-x2)^2 + (y1-y2)^2)
}

# Obtain the distance between rings. Coordinates are associated with the inner
# boundary of a ring, so we obtain values for rows n:(n-1).
get_widths <- function(distances) {
    c(distances[2:length(distances)], NA) - distances
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
                        ring.dist=sdist(x1, y1, x, y)*2.54,
                        ring.width=get_widths(ring.dist))
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
