## species_count.R
## ---------------
## replication counts and data integrity checks

library(plyr)

# read tree data
trees <- read.csv("../data/masters_trees.csv")

# cores
ddply(subset(trees, condition=="alive" & core.taken=="Y"), .(mtn, spcode), 
      summarize,
      N = length(tag))
# EFW: clean up look of code. You need a wrapper or stop at ~80 characters to 
# improve readability

# needles
ddply(subset(trees, needles.collected=="TRUE"), .(mtn, spcode), 
      summarize,
      N = length(tag))

# EFW: Testing how BAF is effected by mountain range and species

baf.lm <- lm(BAF ~ mtn + spcode + mtn:spcode, data=trees)
summary(baf.lm)
anova(baf.lm)
