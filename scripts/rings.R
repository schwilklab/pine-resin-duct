# rings.R

# Read in tree ring coordinates from individual csv files, calculate ring
# widths and areas. Provides data frame "ring_data" to global namespace.

library(dplyr)
library(ggplot2)

trees <- read.csv("../data/masters_trees.csv", stringsAsFactors=FALSE) # needed for tree age
## DWS: How tree age?

SAMPLE_YEAR <- 2015 # year trees were cored, so last full ring should be this
                    # minus one

sdist <- function(x1,y1,x2,y2) {
    sqrt( (x1-x2)^2 + (y1-y2)^2)
}


get_widths <- function(distances) {
    c(distances[2:length(distances)], NA) - distances
}

ring_files <- list.files("../data/tree_ring_coordinates")
ring_data = data.frame()
for (f in ring_files) {
    df <- read.csv(file.path("../data/tree_ring_coordinates", f),
                   stringsAsFactors=FALSE)
    names(df) <- c("ring", "x", "y", "resin.duct.count")
    df$tag <- strsplit(f, "\\.")[[1]][1]
    x1 <- df$x[1]
    y1 <- df$y[1]
    df <- df %>% mutate(calendar.year=SAMPLE_YEAR + ring - max(ring), #or +1?
                        age=max(ring)-ring,
                        # Convert value obtained from inches to cm
                        ring.dist=sdist(x1, y1, x, y)*2.54,
                        ring.width=get_widths(ring.dist))
    ring_data <- rbind(ring_data, df)
}

# clean up
rm(ring_files, df, f, x)
ring_data <- ring_data %>% subset(!is.na(ring.width)) %>%
    inner_join(trees)

# Exploring data
ggplot(ring_data, aes(age, resin.duct.count/ring.width)) +
    geom_point() +
    facet_grid(spcode ~ .)
