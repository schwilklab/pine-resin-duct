# This script uses calculations from ring_data to create new data
# frames and create graphs looking at resin duct densities. This script
# is rough and will need to be cleaned up eventually, and possibly have
# components from it added into an existing .R file.

source("./read_rings.R")
source("./graph-themes.R")

# Calculate summaries per tree
trees.sum <- ring_data %>% group_by(tag) %>%
    summarize(avg.age = mean(age),
              age.sd = sd(age),
              age.min = min(age), max.age = max(age),
              duct.count.mean = mean(resin.duct.count, na.rm= TRUE),
              duct.count.sd = sd(resin.duct.count, na.rm = TRUE),
              duct.den.mean = mean(duct.density, na.rm= TRUE),
              duct.den.sd = sd(duct.density, na.rm= TRUE),
              ring.width.mean = mean(ring.width),
              ring.width.sd = sd(ring.width),
              bai.mean = mean(bai),
              bai.sd = sd(bai),
              precip.mean = mean(PRECIP, na.rm = TRUE),
              precip.sd = sd(PRECIP, na.rm = TRUE)
              ) %>% inner_join(trees)

# Plot mean resin duct count per year
ggplot(trees.sum, aes(spcode, duct.count.mean, fill=mtn)) +
  geom_violin()+
  labs(x= "Species",
       y= "Mean resin duct count per year")+
  scale_x_discrete(labels= species_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(name= "Mountain range",
                    labels = mountain_names,
                    values= mycolours)

# No evidence of strong age effect
ggplot(ring_data, aes(age, duct.density, color=mtn)) +
    facet_grid(spcode ~ ., labeller = as_labeller(species_names)) +
    geom_point() +
    geom_jitter() +
    labs(x = "Age (years)",
         y = "Resin duct density (resin ducts/area)")

ggplot(trees.sum, aes(spcode, duct.den.mean, color=mtn)) +
    geom_violin()

# Plot bai for species
ggplot(trees.sum, aes(spcode, bai.mean, color=mtn)) +
  geom_violin()+
  labs(x= "Species",
       y= "Mean basal area increment")
  

#  geom_pointrange(aes(ymin=avg.resin.year-sem, ymax=avg.resin.year+sem), size = 1)+
#  labs(x= "Species", y= "Mean Resin Duct Count Per Year +/- SEM")


# Plot average age for each species by mountain range
ggplot(avg.age, aes(spcode, avg.age, color=mtn)) + 
  geom_pointrange(aes(ymin=avg.age-sem.age, ymax=avg.age+sem.age), size = 1)+
  labs(x= "Species", y= "Mean Age +/- SEM")


# does BAF predict counts?
ggplot(trees.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)
# looks like weak signal.  Test with linear model:
library(nlme)
baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod)
anova(baf.mod)
# yes, but could easily be just a ring area effect. need to add basal area
# increment to ring_data and add ring area to summary.

# Lets see if we get result using density:
baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod2)
anova(baf.mod2)

# nope, ring widths?
baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod3)
anova(baf.mod3)
# yes:
ggplot(trees.sum, aes(BAF, ring.width.mean, color=mtn)) +
    facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
    theme(strip.text.y = element_text(size=5)) +
    geom_point() +
    labs(x= "Basal area factor",
         y= "Mean ring width")+
    scale_color_manual(name= "Mountain range",
                    labels = mountain_names,
                    values= mycolours)
# ok so that is reassuring

# Try for bai.mean
baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod4)
anova(baf.mod4)
# P = .003

# Resin duct density by tree age
age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn/spcode, data=trees.sum)
summary(age.mod1)
anova(age.mod1)
# P= .0385 

# quick plot showing duct density plotted by tree age
ggplot(trees.sum, aes(max.age, duct.den.mean)) +
  geom_point()

baf.mod5 <- lme(duct.den.mean ~ precip.mean, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod5)
anova(baf.mod5)

ggplot(trees.sum, aes(precip.mean, duct.den.mean)) +
  geom_point()

