## species_count.R
## ---------------
## replication counts and data integrity checks

library(plyr)

# read tree data
trees <- read.csv("../data/masters_trees.csv")

# cores
ddply(subset(trees, condition=="alive" & core.taken=="Y" & pith=="Y"), .(mtn, spcode), summarize, N = length(tag))

# needles
ddply(subset(trees, needles.collected=="TRUE"), .(mtn, spcode), summarize, N = length(tag))
