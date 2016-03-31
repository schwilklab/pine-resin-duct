# This script uses calculations from ring_data to create new data
# frames and create graphs looking at resin duct densities. This script
# is rough and will need to be cleaned up eventually, and possibly have
# components from it added into an existing .R file.

source("./data-checks.R")
source("./rings.R")


# Creates temporary data frame to obtain average, minimum and maximum
# ages for each tree species by mountain range
t<- distinct(ring_data, tag, age ==max(age))
# EL011 was reported twice.  Presumably due to the same y value for 
# the first two years. Hard removal of the incorrect row.
t <- t[-c(7), ]

# Calculate average age, standard deviation and standard error of mean for
# each species
tree.ages<-ddply(subset(t), 
               .(mtn, spcode), plyr::summarize, avg.age = mean(age),
               sd.age = sd(age), sem.age= sd(age)/sqrt(length(spcode)),
               min.age = min(age), max.age = max(age))

# Calculate mean resin ducts per year for each individual
resin.duct.year.tag<- ddply(subset(ring_data), 
               .(mtn, spcode, tag), plyr::summarize,
               avg.resin.year = sum((resin.duct.count)/length(tag), na.rm= TRUE),
               sd = sd(resin.duct.count, na.rm = TRUE),
               sem= sd(resin.duct.count, na.rm = TRUE)/sqrt(length(tag)))

# Calculate mean resin ducts per year for each species
resin.duct.year.species<- ddply(subset(ring_data), 
                       .(mtn, spcode), plyr::summarize,
                       avg.resin.year = sum((resin.duct.count)/length(tag), na.rm= TRUE),
                       sd = sd(resin.duct.count, na.rm = TRUE),
                       sem= sd(resin.duct.count, na.rm = TRUE)/sqrt(length(spcode)))


# Plot mean resin duct count per year by tag
ggplot(resin.duct.year.tag, aes(spcode, avg.resin.year, color=mtn)) +
  geom_violin()+
  labs(x= "Species", y= "Mean Resin Duct Count Per Year
       By Species")

ggplot(ring_data, aes(spcode, resin.duct.count, color=mtn)) +
  geom_violin()+
  labs(x= "Species", y= "Mean Resin Duct Count Per Year
       By Species")

# Plot mean resin duct count per year by species 
ggplot(resin.duct.year.species, aes(spcode, avg.resin.year, color=mtn)) +
  geom_pointrange(aes(ymin=avg.resin.year-sem, ymax=avg.resin.year+sem), size = 1)+
  labs(x= "Species", y= "Mean Resin Duct Count Per Year +/- SEM")


# Plot average age for each species by mountain range
ggplot(avg.age, aes(spcode, avg.age, color=mtn)) + 
  geom_pointrange(aes(ymin=avg.age-sem.age, ymax=avg.age+sem.age), size = 1)+
  labs(x= "Species", y= "Mean Age +/- SEM")
