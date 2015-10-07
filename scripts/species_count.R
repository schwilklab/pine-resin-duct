## species_count.R
## ---------------
## replication counts and data integrity checks

library(plyr)

# read tree data
trees <- read.csv("../data/masters_trees.csv")

# cores
ddply(subset(trees, condition=="alive" & core.taken=="Y" & pith=="Y"), 
      .(mtn, spcode), summarize, N = length(tag))

# needles
ddply(subset(trees, needles.collected=="TRUE"), .(mtn, spcode),
      summarize, N = length(tag))

# EFW: Clean up code. Want to include either a wrapper or make sure your code
# Doesn't go past 80 characters to improve readability. Here's 80 ------------!


# EFW: Added in analysis of BAF. Simple linear regression followed by a ANOVA

baf.lm <- lm(BAF~ mtn + spcode + spcode:mtn, data=trees)
summary(baf.lm)
anova(baf.lm)

# There is a difference between species.  Looking at figure to get better idea

library(ggplot2)

ggplot(trees, aes(spcode, BAF, color=mtn)) +
  geom_point()