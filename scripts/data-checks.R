## data-checks.R
## ---------------
## replication counts and data integrity checks


# Make sure graph-themes.R is loaded. If not:
# source("./graph-themes.R")

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

mycolours <- c("#FF6600", "#CC0000", "#660066", "#000999", "#3366CC", "#00CCFF")

ggplot(trees, aes(spcode, BAF, fill=mtn)) +
  geom_violin()+
  labs(x= "Species", y= "Basal area factor")+
  scale_x_discrete(labels= species_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(name= "Mountain range",
                    labels = mountain_names,
                    values= mycolours)


# Linear regression of DBH followed by ANOVA
dbh.lm <- lm(DBH~ mtn + spcode + spcode:mtn, data=trees)
summary(dbh.lm)
anova(dbh.lm)

#DBH plotted by species and mountain range
ggplot(trees, aes(spcode, DBH, color=mtn)) + 
  geom_violin()+
  theme_light()
