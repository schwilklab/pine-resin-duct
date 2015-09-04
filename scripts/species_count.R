## species_count.R
## ---------------
## replication counts and data integrity checks

library(plyr)

# read tree data
trees <- read.csv("../data/masters_trees.csv")

# cores
ddply(subset(trees, condition=="alive"), .(mtn, spcode), summarize, N = length(tag))

# needles
ddply(subset(trees, needles.collected=="Y"), .(mtn, spcode), summarize, N = length(tag))
# DWS: note that you should really use boolean variables for boolean data. Why
# is needles.collected not TRUE/FALSE?
