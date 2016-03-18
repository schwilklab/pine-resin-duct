## species_count.R
## ---------------
## replication counts and data integrity checks

# Clears the workspace
rm(list=ls())

# Load programs
library(plyr)
library(ggplot2)

# read tree data
trees <- read.csv("../data/masters_trees.csv")

# cores
ddply(subset(trees, core.taken=="Y" & pith=="Y"), 
      .(mtn, spcode), summarize, N = length(tag))

# needles
ddply(subset(trees, needles.collected=="TRUE"), .(mtn, spcode),
      summarize, N = length(tag))

# Linear regression of BAF followed by ANOVA
baf.lm <- lm(BAF~ mtn + spcode + spcode:mtn, data=trees)
summary(baf.lm)
anova(baf.lm)

# There is a difference between species.  Looking at figure to get better idea
ggplot(trees, aes(spcode, BAF, color=mtn)) +
  geom_violin()

# Linear regression of DBH followed by ANOVA
dbh.lm <- lm(DBH~ mtn + spcode + spcode:mtn, data=trees)
summary(dbh.lm)
anova(dbh.lm)

#DBH plotted by species and mountain range
ggplot(trees, aes(spcode, DBH, color=mtn)) + 
  geom_violin()
