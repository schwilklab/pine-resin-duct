# rings.R

# Read in tree ring coordinates from individual csv files, calcualte ring
# widths and areas. Provides data frame "ring_data" to global namespace

library(plyr)

sdist <- function(x1,y1,x2,y2) {
    sqrt( (x1-x2)^2 + (y1-y2)^2)
}


get_widths <- function(distances) {
    c(distances[2:length(distances)], NA) - distances
}

ring_files <- list.files("../data/tree_ring_coordinates", pattern = "*.csv")
ring_data = data.frame()
for (f in ring_files) {
    df <- read.csv(file.path("../data/tree_ring_coordinates", f))
    names(df) <- c("ring", "x", "y")
    df$tag <- strsplit(f, "\\.")[[1]][1]
    # "ring number/id" is count from center of bole.
    df$ring.dist <- sdist(df$x[1],df$y[1], df$x, df$y)
    df$ring.width <- get_widths(df$ring.dist)
    df <- df[-nrow(df),]
    ring_data <- rbind.fill(ring_data, df)
}

# clean up
rm(ring_files)
