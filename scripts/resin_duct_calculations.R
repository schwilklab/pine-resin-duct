## Resin_duct_calculations

# This script uses calculations from ring_data to create new data
# frames and create graphs looking at resin duct densities. This script
# is rough and will need to be cleaned up eventually, and possibly have
# components from it added into an existing .R file.  Multiple linear
# models were run with the outputs from the p value extracted into a 
# csv file manually. This script is very long, so to look at the data
# already calculated, check out lme_outputs which contains all of the 
# information. This can be cleaned up a lot, but it's a start.

source("./graph-themes.R")
source("./read_rings.R")
library(nlme)
library(tidyr)

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

# Create seperate data sets subsetted by species to look for individual
# species effect
pice.sum <- filter(trees.sum, spcode == ("PICE"))
pied.sum <- filter(trees.sum, spcode == ("PIED"))
pipo.sum <- filter(trees.sum, spcode == ("PIPO"))
piar5.sum <- filter(trees.sum, spcode == ("PIAR5"))
pist3.sum <- filter(trees.sum, spcode == ("PIST3"))

# Combine ponderosa type pines together and pinyon pines together for 
# further analyses
ponderosa.sum<- bind_rows(pipo.sum, piar5.sum)
pinyon.sum<- bind_rows(pice.sum, pied.sum)

# I created a .csv file with the pvalues of each linear model that was
# run.  I know I probably took a very inefficient way to obtain the outputs
# but I can fix the code up later.  
lme_outputs <- read.csv("../data/lme_anova_pvalue_trees.csv")
lme_significant_outputs <- filter(lme_outputs, p_value <= .05)

## Plotting trends in data

# Resin duct count plotted by year
ggplot(trees.sum, aes(spcode, duct.count.mean, fill=mtn)) +
  geom_violin()+
  labs(x= "Species",
       y= "Mean resin duct count per year")+
  scale_x_discrete(labels= species_names)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(name= "Mountain range",
                    labels = mountain_names,
                    values= mycolours)

# Resin duct density plotted over age and faceted by mountain range
ggplot(ring_data, aes(age, duct.density, color=mtn)) +
  facet_grid(spcode ~ ., labeller = as_labeller(species_names)) +
  geom_point() +
  geom_jitter() +
  labs(x = "Age (years)",
       y = "Resin duct density (resin ducts/area)")

# Mean resin duct density plotted by species in each mountain range
ggplot(trees.sum, aes(spcode, duct.den.mean, color=mtn)) +
  geom_violin()

# Basal area increment plotted by species in each mountain range
ggplot(trees.sum, aes(spcode, bai.mean, color=mtn)) +
  geom_violin()+
  labs(x= "Species",
       y= "Mean basal area increment")

# Average age of each species plotted acrosss mountain range
ggplot(trees.sum, aes(spcode, avg.age, color=mtn)) + 
  geom_pointrange(aes(ymin=avg.age-age.sd, ymax=avg.age+age.sd), size = 1)+
  labs(x= "Species", y= "Mean Age +/- SEM")
# This isn't representative of age well, should change the plot

### Exploring some questions ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(trees.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 1: BAF and mean resin duct count
baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod)
anova(baf.mod)
# P = .0016

# Linear model 2: BAF and mean resin duct density
baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod2)
anova(baf.mod2)
# P = .287

# Linear model 3: BAF and mean ring width
baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod3)
anova(baf.mod3)
# P = .0013

# Plot linear model 3
ggplot(trees.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 4: BAF and mean basal area increment
baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn/spcode, data=trees.sum)
summary(baf.mod4)
anova(baf.mod4)
# P = .003
# This makes sense because it should be very similar to ring width

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 5: Tree age and mean resin duct density
age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn/spcode, data=trees.sum)
summary(age.mod1)
anova(age.mod1)
# P= .0385 

# Plot linear model 5:
ggplot(trees.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 6: Tree age and mean resin duct count
age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn/spcode, data=trees.sum)
summary(age.mod2)
anova(age.mod2)
# P <.0001
# This is a "duh" one

# Linear model 7: Tree age and ring width
age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn/spcode, data=trees.sum)
summary(age.mod3)
anova(age.mod3)
# P <.0001
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 8: Tree age and basal area increment
age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn/spcode, data=trees.sum)
summary(age.mod4)
anova(age.mod4)
# P = .3039
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 9: Elevation and mean resin duct density
elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn/spcode, data=trees.sum)
summary(elev.mod1)
anova(elev.mod1)
# p = .6788

# Linear model 10: Elevation and mean resin duct count
elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn/spcode, data=trees.sum)
summary(elev.mod2)
anova(elev.mod2)
# P = .6455

# Linear model 11: Elevation and ring width
elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn/spcode, data=trees.sum)
summary(elev.mod3)
anova(elev.mod3)
# P =.8988

# Linear model 12: Elevation and basal area increment
elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn/spcode, data=trees.sum)
summary(elev.mod4)
anova(elev.mod4)
# P = .9432

# So it looks like elevation doesn't have an overall effect

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 13: Radiation and mean resin duct density
rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn/spcode, data=trees.sum)
summary(rad.mod1)
anova(rad.mod1)
# p = .5738

# Linear model 14: Radiation and mean resin duct count
rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn/spcode, data=trees.sum)
summary(rad.mod2)
anova(rad.mod2)
# P = .4763

# Linear model 15: Radiation and ring width
rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn/spcode, data=trees.sum)
summary(rad.mod3)
anova(rad.mod3)
# P =.1147

# Linear model 16: Radiation and basal area increment
rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn/spcode, data=trees.sum)
summary(rad.mod4)
anova(rad.mod4)
# P = .0403

# Interesting.  Radiation apparantly does have an effect on basal area
# increment based on these values.  Will need to run again once updated
# values are  obtained, though.

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 17: Slope and mean resin duct density
slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn/spcode, data=trees.sum)
summary(slope.mod1)
anova(slope.mod1)
# p = .4328

# Linear model 18: Slope and mean resin duct count
slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn/spcode, data=trees.sum)
summary(slope.mod2)
anova(slope.mod2)
# P = .005
# Hmmm, I wonder what wouldd contribute toward a higher count,but not 
# density.

# Linear model 19: Slope and ring width
slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn/spcode, data=trees.sum)
summary(slope.mod3)
anova(slope.mod3)
# P =.0498

# Linear model 20: Slope and basal area increment
slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn/spcode, data=trees.sum)
summary(slope.mod4)
anova(slope.mod4)
# P = .6342

# So it looks like there's a significant difference in resin duct count,
# but not density.  And just barely a signal in ring width, but not BAI.
# Will need to think about this more.

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 21: Downstream distance and mean resin duct density
ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn/spcode, data=trees.sum)
summary(ldv2.mod1)
anova(ldv2.mod1)
# p = .9693

# Linear model 22: Downstream distance and mean resin duct count
ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn/spcode, data=trees.sum)
summary(ldv2.mod2)
anova(ldv2.mod2)
# P = .0027

# Linear model 23: Downstream distance and ring width
ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn/spcode, data=trees.sum)
summary(ldv2.mod3)
anova(ldv2.mod3)
# P =.0039

# Linear model 24: Downstream distance and basal area increment
ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn/spcode, data=trees.sum)
summary(ldv2.mod4)
anova(ldv2.mod4)
# P = .2416

### Exploring some questions for ponderosa pines (arizonica and ponderosa) ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(ponderosa.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 25: BAF and mean resin duct count
pond.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=ponderosa.sum)
summary(pond.baf.mod)
anova(pond.baf.mod)
# P = .0032

# Linear model 26: BAF and mean resin duct density
pond.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=ponderosa.sum)
summary(pond.baf.mod2)
anova(pond.baf.mod2)
# P = .164

# Linear model 27: BAF and mean ring width
pond.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=ponderosa.sum)
summary(pond.baf.mod3)
anova(pond.baf.mod3)
# P = .002

# Plot linear model 27
ggplot(ponderosa.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 28: BAF and mean basal area increment
pond.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=ponderosa.sum)
summary(pond.baf.mod4)
anova(pond.baf.mod4)
# P = .0054
# This makes sense because it should be very similar to ring width

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 29: Tree age and mean resin duct density
pond.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=ponderosa.sum)
summary(pond.age.mod1)
anova(pond.age.mod1)
# P= .3195

# Plot linear model 5:
ggplot(ponderosa.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 30: Tree age and mean resin duct count
pond.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=ponderosa.sum)
summary(pond.age.mod2)
anova(pond.age.mod2)
# P <.0001
# This is a "duh" one

# Linear model 31: Tree age and ring width
pond.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=ponderosa.sum)
summary(pond.age.mod3)
anova(pond.age.mod3)
# P <.0012
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 32: Tree age and basal area increment
pond.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=ponderosa.sum)
summary(pond.age.mod4)
anova(pond.age.mod4)
# P = .399
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 33: Elevation and mean resin duct density
pond.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=ponderosa.sum)
summary(pond.elev.mod1)
anova(pond.elev.mod1)
# p = .491

# Linear model 34: Elevation and mean resin duct count
pond.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=ponderosa.sum)
summary(pond.elev.mod2)
anova(pond.elev.mod2)
# P = .3431

# Linear model 35: Elevation and ring width
pond.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=ponderosa.sum)
summary(pond.elev.mod3)
anova(pond.elev.mod3)
# P =.3957

# Linear model 36: Elevation and basal area increment
pond.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=ponderosa.sum)
summary(pond.elev.mod4)
anova(pond.elev.mod4)
# P = .4295

# So it looks like elevation doesn't have an overall effect

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 37: Radiation and mean resin duct density
pond.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=ponderosa.sum)
summary(pond.rad.mod1)
anova(pond.rad.mod1)
# p = .8875

# Linear model 38: Radiation and mean resin duct count
pond.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=ponderosa.sum)
summary(pond.rad.mod2)
anova(pond.rad.mod2)
# P = .5752

# Linear model 39: Radiation and ring width
pond.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=ponderosa.sum)
summary(pond.rad.mod3)
anova(pond.rad.mod3)
# P =.567

# Linear model 40: Radiation and basal area increment
pond.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=ponderosa.sum)
summary(pond.rad.mod4)
anova(pond.rad.mod4)
# P = .3231

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 41: Slope and mean resin duct density
pond.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=ponderosa.sum)
summary(pond.slope.mod1)
anova(pond.slope.mod1)
# p = .927

# Linear model 42: Slope and mean resin duct count
pond.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=ponderosa.sum)
summary(pond.slope.mod2)
anova(pond.slope.mod2)
# P = .0328
# Hmmm, I wonder what would contribute toward a higher count,but not 
# density.

# Linear model 43: Slope and ring width
pond.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=ponderosa.sum)
summary(pond.slope.mod3)
anova(pond.slope.mod3)
# P =.1402

# Linear model 44: Slope and basal area increment
pond.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=ponderosa.sum)
summary(pond.slope.mod4)
anova(pond.slope.mod4)
# P = .4783

# So it looks like there's a significant difference in resin duct count,
# but not density.

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 45: Downstream distance and mean resin duct density
pond.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=ponderosa.sum)
summary(pond.ldv2.mod1)
anova(pond.ldv2.mod1)
# p = .4989

# Linear model 46: Downstream distance and mean resin duct count
pond.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=ponderosa.sum)
summary(pond.ldv2.mod2)
anova(pond.ldv2.mod2)
# P = .0012

# Linear model 47: Downstream distance and ring width
pond.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=ponderosa.sum)
summary(pond.ldv2.mod3)
anova(pond.ldv2.mod3)
# P =.0047

# Linear model 48: Downstream distance and basal area increment
pond.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=ponderosa.sum)
summary(pond.ldv2.mod4)
anova(pond.ldv2.mod4)
# P = .1566

# Similar results to slope in terms of deemed signficance from variables
# according to the model.

### Exploring some questions for pinyon (cembroides and edulis) pines ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(pinyon.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 49: BAF and mean resin duct count
piny.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=pinyon.sum)
summary(piny.baf.mod)
anova(piny.baf.mod)
# P = .813

# Linear model 50: BAF and mean resin duct density
piny.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=pinyon.sum)
summary(piny.baf.mod2)
anova(piny.baf.mod2)
# P = .2439

# Linear model 51: BAF and mean ring width
piny.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=pinyon.sum)
summary(piny.baf.mod3)
anova(piny.baf.mod3)
# P = .5596

# Plot linear model 51
ggplot(pinyon.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 52: BAF and mean basal area increment
piny.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=pinyon.sum)
summary(piny.baf.mod4)
anova(piny.baf.mod4)
# P = .3276

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 53: Tree age and mean resin duct density
piny.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=pinyon.sum)
summary(piny.age.mod1)
anova(piny.age.mod1)
# P= .1916

# Plot linear model 53:
ggplot(pinyon.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 54: Tree age and mean resin duct count
piny.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=pinyon.sum)
summary(piny.age.mod2)
anova(piny.age.mod2)
# P <.0001
# This is a "duh" one

# Linear model 55: Tree age and ring width
piny.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=pinyon.sum)
summary(piny.age.mod3)
anova(piny.age.mod3)
# P <.0001
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 56: Tree age and basal area increment
piny.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=pinyon.sum)
summary(piny.age.mod4)
anova(piny.age.mod4)
# P = .365
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 57: Elevation and mean resin duct density
piny.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=pinyon.sum)
summary(piny.elev.mod1)
anova(piny.elev.mod1)
# p = .4604

# Linear model 58: Elevation and mean resin duct count
piny.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=pinyon.sum)
summary(piny.elev.mod2)
anova(piny.elev.mod2)
# P = .1366

# Linear model 59: Elevation and ring width
piny.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=pinyon.sum)
summary(piny.elev.mod3)
anova(piny.elev.mod3)
# P =.0849

# Linear model 60: Elevation and basal area increment
piny.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=pinyon.sum)
summary(piny.elev.mod4)
anova(piny.elev.mod4)
# P = .0393

# Hmmmm, so elevation does have an effect on basal area increment for
# pinyon pines.  Interesting.  Is this seen in other species?

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 61: Radiation and mean resin duct density
piny.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=pinyon.sum)
summary(piny.rad.mod1)
anova(piny.rad.mod1)
# p = .6757

# Linear model 62: Radiation and mean resin duct count
piny.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=pinyon.sum)
summary(piny.rad.mod2)
anova(piny.rad.mod2)
# P = .929

# Linear model 63: Radiation and ring width
piny.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=pinyon.sum)
summary(piny.rad.mod3)
anova(piny.rad.mod3)
# P =.3283

# Linear model 64: Radiation and basal area increment
piny.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=pinyon.sum)
summary(piny.rad.mod4)
anova(piny.rad.mod4)
# P = .2059

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 65: Slope and mean resin duct density
piny.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=pinyon.sum)
summary(piny.slope.mod1)
anova(piny.slope.mod1)
# p = .1787

# Linear model 66: Slope and mean resin duct count
piny.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=pinyon.sum)
summary(piny.slope.mod2)
anova(piny.slope.mod2)
# P = .0714

# Linear model 67: Slope and ring width
piny.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=pinyon.sum)
summary(piny.slope.mod3)
anova(piny.slope.mod3)
# P =.0155

# Linear model 68: Slope and basal area increment
piny.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=pinyon.sum)
summary(piny.slope.mod4)
anova(piny.slope.mod4)
# P = .1845

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 69: Downstream distance and mean resin duct density
piny.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=pinyon.sum)
summary(piny.ldv2.mod1)
anova(piny.ldv2.mod1)
# p = .279

# Linear model 70: Downstream distance and mean resin duct count
piny.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=pinyon.sum)
summary(piny.ldv2.mod2)
anova(piny.ldv2.mod2)
# P = .7744

# Linear model 71: Downstream distance and ring width
piny.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=pinyon.sum)
summary(piny.ldv2.mod3)
anova(piny.ldv2.mod3)
# P =.5477

# Linear model 72: Downstream distance and basal area increment
piny.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=pinyon.sum)
summary(piny.ldv2.mod4)
anova(piny.ldv2.mod4)
# P = .1536

### Exploring some questions for Pinus strobiformis ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(pist3.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 73: BAF and mean resin duct count
pist3.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=pist3.sum)
summary(pist3.baf.mod)
anova(pist3.baf.mod)
# P = .0748

# Linear model 74: BAF and mean resin duct density
pist3.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=pist3.sum)
summary(pist3.baf.mod2)
anova(pist3.baf.mod2)
# P = .7566

# Linear model 75: BAF and mean ring width
pist3.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=pist3.sum)
summary(pist3.baf.mod3)
anova(pist3.baf.mod3)
# P = .1175

# Plot linear model 75
ggplot(pist3.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 76: BAF and mean basal area increment
pist3.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=pist3.sum)
summary(pist3.baf.mod4)
anova(pist3.baf.mod4)
# P = .0727
# This makes sense because it should be very similar to ring width

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 77: Tree age and mean resin duct density
pist3.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=pist3.sum)
summary(pist3.age.mod1)
anova(pist3.age.mod1)
# P= .425

# Plot linear model 77:
ggplot(pist3.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 78: Tree age and mean resin duct count
pist3.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=pist3.sum)
summary(pist3.age.mod2)
anova(pist3.age.mod2)
# P = .0048
# This is a "duh" one

# Linear model 79: Tree age and ring width
pist3.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=pist3.sum)
summary(pist3.age.mod3)
anova(pist3.age.mod3)
# P = .0294
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 80: Tree age and basal area increment
pist3.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=pist3.sum)
summary(pist3.age.mod4)
anova(pist3.age.mod4)
# P = .6675
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 81: Elevation and mean resin duct density
pist3.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=pist3.sum)
summary(pist3.elev.mod1)
anova(pist3.elev.mod1)
# p = .9709

# Linear model 82: Elevation and mean resin duct count
pist3.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=pist3.sum)
summary(pist3.elev.mod2)
anova(pist3.elev.mod2)
# P = .2335

# Linear model 83: Elevation and ring width
pist3.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=pist3.sum)
summary(pist3.elev.mod3)
anova(pist3.elev.mod3)
# P =.0990

# Linear model 84: Elevation and basal area increment
pist3.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=pist3.sum)
summary(pist3.elev.mod4)
anova(pist3.elev.mod4)
# P = .0440

# Hmmmm, so elevation does have an effect on basal area increment for
# strobiformis pines.  Interesting.

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 85: Radiation and mean resin duct density
pist3.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=pist3.sum)
summary(pist3.rad.mod1)
anova(pist3.rad.mod1)
# p = .4382

# Linear model 86: Radiation and mean resin duct count
pist3.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=pist3.sum)
summary(pist3.rad.mod2)
anova(pist3.rad.mod2)
# P = .0513

# Linear model 87: Radiation and ring width
pist3.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=pist3.sum)
summary(pist3.rad.mod3)
anova(pist3.rad.mod3)
# P =.0463

# Linear model 88: Radiation and basal area increment
pist3.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=pist3.sum)
summary(pist3.rad.mod4)
anova(pist3.rad.mod4)
# P = .0642

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 89: Slope and mean resin duct density
pist3.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=pist3.sum)
summary(pist3.slope.mod1)
anova(pist3.slope.mod1)
# p = .381

# Linear model 90: Slope and mean resin duct count
pist3.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=pist3.sum)
summary(pist3.slope.mod2)
anova(pist3.slope.mod2)
# P = .9438

# Linear model 91: Slope and ring width
pist3.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=pist3.sum)
summary(pist3.slope.mod3)
anova(pist3.slope.mod3)
# P =.4866

# Linear model 92: Slope and basal area increment
pist3.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=pist3.sum)
summary(pist3.slope.mod4)
anova(pist3.slope.mod4)
# P = .4119

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 93: Downstream distance and mean resin duct density
pist3.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=pist3.sum)
summary(pist3.ldv2.mod1)
anova(pist3.ldv2.mod1)
# p = .7784

# Linear model 94: Downstream distance and mean resin duct count
pist3.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=pist3.sum)
summary(pist3.ldv2.mod2)
anova(pist3.ldv2.mod2)
# P = .0445

# Linear model 95: Downstream distance and ring width
pist3.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=pist3.sum)
summary(pist3.ldv2.mod3)
anova(pist3.ldv2.mod3)
# P =.0627

# Linear model 96: Downstream distance and basal area increment
pist3.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=pist3.sum)
summary(pist3.ldv2.mod4)
anova(pist3.ldv2.mod4)
# P = .4104

### Exploring some questions for Pinus cembroides ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(pice.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 97: BAF and mean resin duct count
pice.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=pice.sum)
summary(pice.baf.mod)
anova(pice.baf.mod)
# P = .3032

# Linear model 98: BAF and mean resin duct density
pice.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=pice.sum)
summary(pice.baf.mod2)
anova(pice.baf.mod2)
# P = .2458

# Linear model 99: BAF and mean ring width
pice.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=pice.sum)
summary(pice.baf.mod3)
anova(pice.baf.mod3)
# P = .1676

# Plot linear model 99
ggplot(pice.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 100: BAF and mean basal area increment
pice.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=pice.sum)
summary(pice.baf.mod4)
anova(pice.baf.mod4)
# P = .5558

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 101: Tree age and mean resin duct density
pice.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=pice.sum)
summary(pice.age.mod1)
anova(pice.age.mod1)
# P= .2935

# Plot linear model 101:
ggplot(pice.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 102: Tree age and mean resin duct count
pice.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=pice.sum)
summary(pice.age.mod2)
anova(pice.age.mod2)
# P = .0042
# This is a "duh" one

# Linear model 103: Tree age and ring width
pice.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=pice.sum)
summary(pice.age.mod3)
anova(pice.age.mod3)
# P = .0205
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 104: Tree age and basal area increment
pice.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=pice.sum)
summary(pice.age.mod4)
anova(pice.age.mod4)
# P = .3911
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 105: Elevation and mean resin duct density
pice.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=pice.sum)
summary(pice.elev.mod1)
anova(pice.elev.mod1)
# p = .1382

# Linear model 106: Elevation and mean resin duct count
pice.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=pice.sum)
summary(pice.elev.mod2)
anova(pice.elev.mod2)
# P = .0015

# Linear model 107: Elevation and ring width
pice.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=pice.sum)
summary(pice.elev.mod3)
anova(pice.elev.mod3)
# P =.0001

# Linear model 108: Elevation and basal area increment
pice.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=pice.sum)
summary(pice.elev.mod4)
anova(pice.elev.mod4)
# P = .0031

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 109: Radiation and mean resin duct density
pice.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=pice.sum)
summary(pice.rad.mod1)
anova(pice.rad.mod1)
# p = .7878

# Linear model 110: Radiation and mean resin duct count
pice.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=pice.sum)
summary(pice.rad.mod2)
anova(pice.rad.mod2)
# P = .4941

# Linear model 111: Radiation and ring width
pice.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=pice.sum)
summary(pice.rad.mod3)
anova(pice.rad.mod3)
# P =.5966

# Linear model 112: Radiation and basal area increment
pice.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=pice.sum)
summary(pice.rad.mod4)
anova(pice.rad.mod4)
# P = .2866

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 113: Slope and mean resin duct density
pice.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=pice.sum)
summary(pice.slope.mod1)
anova(pice.slope.mod1)
# p = .4421

# Linear model 114: Slope and mean resin duct count
pice.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=pice.sum)
summary(pice.slope.mod2)
anova(pice.slope.mod2)
# P = .1298

# Linear model 115: Slope and ring width
pice.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=pice.sum)
summary(pice.slope.mod3)
anova(pice.slope.mod3)
# P =.1236

# Linear model 116: Slope and basal area increment
pice.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=pice.sum)
summary(pice.slope.mod4)
anova(pice.slope.mod4)
# P = .9879

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 117: Downstream distance and mean resin duct density
pice.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=pice.sum)
summary(pice.ldv2.mod1)
anova(pice.ldv2.mod1)
# p = .1467

# Linear model 118: Downstream distance and mean resin duct count
pice.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=pice.sum)
summary(pice.ldv2.mod2)
anova(pice.ldv2.mod2)
# P = .2224

# Linear model 119: Downstream distance and ring width
pice.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=pice.sum)
summary(pice.ldv2.mod3)
anova(pice.ldv2.mod3)
# P =.0355

# Linear model 120: Downstream distance and basal area increment
pice.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=pice.sum)
summary(pice.ldv2.mod4)
anova(pice.ldv2.mod4)
# P = .0352

### Exploring some questions for Pinus edulis ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(pied.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 121: BAF and mean resin duct count
pied.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=pied.sum)
summary(pied.baf.mod)
anova(pied.baf.mod)
# P = .1233

# Linear model 122: BAF and mean resin duct density
pied.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=pied.sum)
summary(pied.baf.mod2)
anova(pied.baf.mod2)
# P = .9508

# Linear model 123: BAF and mean ring width
pied.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=pied.sum)
summary(pied.baf.mod3)
anova(pied.baf.mod3)
# P = .1676

# Plot linear model 123
ggplot(pied.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 124: BAF and mean basal area increment
pied.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=pied.sum)
summary(pied.baf.mod4)
anova(pied.baf.mod4)
# P = .6000

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 125: Tree age and mean resin duct density
pied.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=pied.sum)
summary(pied.age.mod1)
anova(pied.age.mod1)
# P= .4859

# Plot linear model 125:
ggplot(pied.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 126: Tree age and mean resin duct count
pied.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=pied.sum)
summary(pied.age.mod2)
anova(pied.age.mod2)
# P = .0085
# This is a "duh" one

# Linear model 127: Tree age and ring width
pied.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=pied.sum)
summary(pied.age.mod3)
anova(pied.age.mod3)
# P = .0074
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 128: Tree age and basal area increment
pied.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=pied.sum)
summary(pied.age.mod4)
anova(pied.age.mod4)
# P = .6313
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 129: Elevation and mean resin duct density
pied.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=pied.sum)
summary(pied.elev.mod1)
anova(pied.elev.mod1)
# p = .9203

# Linear model 130: Elevation and mean resin duct count
pied.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=pied.sum)
summary(pied.elev.mod2)
anova(pied.elev.mod2)
# P = .6184

# Linear model 131: Elevation and ring width
pied.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=pied.sum)
summary(pied.elev.mod3)
anova(pied.elev.mod3)
# P =.7116

# Linear model 132: Elevation and basal area increment
pied.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=pied.sum)
summary(pied.elev.mod4)
anova(pied.elev.mod4)
# P = .5757

# Hmmmm, so elevation does have an effect on basal area increment for
# strobiformis pines.  Interesting.

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 133: Radiation and mean resin duct density
pied.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=pied.sum)
summary(pied.rad.mod1)
anova(pied.rad.mod1)
# p = .3861

# Linear model 134: Radiation and mean resin duct count
pied.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=pied.sum)
summary(pied.rad.mod2)
anova(pied.rad.mod2)
# P = .2527

# Linear model 135: Radiation and ring width
pied.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=pied.sum)
summary(pied.rad.mod3)
anova(pied.rad.mod3)
# P =.5398

# Linear model 136: Radiation and basal area increment
pied.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=pied.sum)
summary(pied.rad.mod4)
anova(pied.rad.mod4)
# P = .6420

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 137: Slope and mean resin duct density
pied.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=pied.sum)
summary(pied.slope.mod1)
anova(pied.slope.mod1)
# p = .2001

# Linear model 138: Slope and mean resin duct count
pied.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=pied.sum)
summary(pied.slope.mod2)
anova(pied.slope.mod2)
# P = .2519

# Linear model 139: Slope and ring width
pied.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=pied.sum)
summary(pied.slope.mod3)
anova(pied.slope.mod3)
# P =.0497

# Linear model 140: Slope and basal area increment
pied.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=pied.sum)
summary(pied.slope.mod4)
anova(pied.slope.mod4)
# P = .002

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 141: Downstream distance and mean resin duct density
pied.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=pied.sum)
summary(pied.ldv2.mod1)
anova(pied.ldv2.mod1)
# p = .9949

# Linear model 142: Downstream distance and mean resin duct count
pied.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=pied.sum)
summary(pied.ldv2.mod2)
anova(pied.ldv2.mod2)
# P = .0181

# Linear model 143: Downstream distance and ring width
pied.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=pied.sum)
summary(pied.ldv2.mod3)
anova(pied.ldv2.mod3)
# P =.0494

# Linear model 144: Downstream distance and basal area increment
pied.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=pied.sum)
summary(pied.ldv2.mod4)
anova(pied.ldv2.mod4)
# P = .6389

### Exploring some questions for Pinus ponderosa ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(pipo.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 145: BAF and mean resin duct count
pipo.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=pipo.sum)
summary(pipo.baf.mod)
anova(pipo.baf.mod)
# P = .0057

# Linear model 146: BAF and mean resin duct density
pipo.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=pipo.sum)
summary(pipo.baf.mod2)
anova(pipo.baf.mod2)
# P = .7285

# Linear model 147: BAF and mean ring width
pipo.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=pipo.sum)
summary(pipo.baf.mod3)
anova(pipo.baf.mod3)
# P = .0147

# Plot linear model 147
ggplot(pipo.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 148: BAF and mean basal area increment
pipo.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=pipo.sum)
summary(pipo.baf.mod4)
anova(pipo.baf.mod4)
# P = .101

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 149: Tree age and mean resin duct density
pipo.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=pipo.sum)
summary(pipo.age.mod1)
anova(pipo.age.mod1)
# P= .0905

# Plot linear model 149:
ggplot(pipo.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 150: Tree age and mean resin duct count
pipo.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=pipo.sum)
summary(pipo.age.mod2)
anova(pipo.age.mod2)
# P = .0009
# This is a "duh" one

# Linear model 151: Tree age and ring width
pipo.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=pipo.sum)
summary(pipo.age.mod3)
anova(pipo.age.mod3)
# P = .0009
# This makes sense as well since as the tree gets older, it is producing 
# thinner rings to account for circumference.  What about BAI?

# Linear model 152: Tree age and basal area increment
pipo.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=pipo.sum)
summary(pipo.age.mod4)
anova(pipo.age.mod4)
# P = .7351
# So there isn't a significant change in overall growth as the tree ages

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 153: Elevation and mean resin duct density
pipo.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=pipo.sum)
summary(pipo.elev.mod1)
anova(pipo.elev.mod1)
# p = .734

# Linear model 154: Elevation and mean resin duct count
pipo.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=pipo.sum)
summary(pipo.elev.mod2)
anova(pipo.elev.mod2)
# P = .6067

# Linear model 155: Elevation and ring width
pipo.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=pipo.sum)
summary(pipo.elev.mod3)
anova(pipo.elev.mod3)
# P =.7758

# Linear model 156: Elevation and basal area increment
pipo.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=pipo.sum)
summary(pipo.elev.mod4)
anova(pipo.elev.mod4)
# P = .9916

# Hmmmm, so elevation does have an effect on basal area increment for
# strobiformis pines.  Interesting.

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 157: Radiation and mean resin duct density
pipo.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=pipo.sum)
summary(pipo.rad.mod1)
anova(pipo.rad.mod1)
# p = .9507

# Linear model 158: Radiation and mean resin duct count
pipo.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=pipo.sum)
summary(pipo.rad.mod2)
anova(pipo.rad.mod2)
# P = .4464

# Linear model 159: Radiation and ring width
pipo.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=pipo.sum)
summary(pipo.rad.mod3)
anova(pipo.rad.mod3)
# P = .4651

# Linear model 160: Radiation and basal area increment
pipo.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=pipo.sum)
summary(pipo.rad.mod4)
anova(pipo.rad.mod4)
# P = .3402

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 161: Slope and mean resin duct density
pipo.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=pipo.sum)
summary(pipo.slope.mod1)
anova(pipo.slope.mod1)
# p = .5831

# Linear model 162: Slope and mean resin duct count
pipo.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=pipo.sum)
summary(pipo.slope.mod2)
anova(pipo.slope.mod2)
# P = .1549

# Linear model 163: Slope and ring width
pipo.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=pipo.sum)
summary(pipo.slope.mod3)
anova(pipo.slope.mod3)
# P =.2526

# Linear model 164: Slope and basal area increment
pipo.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=pipo.sum)
summary(pipo.slope.mod4)
anova(pipo.slope.mod4)
# P = .9605

## 5. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 165: Downstream distance and mean resin duct density
pipo.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=pipo.sum)
summary(pipo.ldv2.mod1)
anova(pipo.ldv2.mod1)
# p = .3829

# Linear model 166: Downstream distance and mean resin duct count
pipo.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=pipo.sum)
summary(pipo.ldv2.mod2)
anova(pipo.ldv2.mod2)
# P = .0072

# Linear model 167: Downstream distance and ring width
pipo.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=pipo.sum)
summary(pipo.ldv2.mod3)
anova(pipo.ldv2.mod3)
# P =.0265

# Linear model 168: Downstream distance and basal area increment
pipo.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=pipo.sum)
summary(pipo.ldv2.mod4)
anova(pipo.ldv2.mod4)
# P = .2072

### Exploring some questions for Pinus arizonica ***

## 1. Does competition have an effect on resin duct traits or growth traits?

# Using BAF as a proxy for competition

# Plotting resin duct count by basal area fastor in each mountain range
ggplot(piar5.sum, aes(BAF, duct.count.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean resin duct count per year")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Test the relationship with a linear model

# Linear model 169: BAF and mean resin duct count
piar5.baf.mod <- lme(duct.count.mean ~ BAF, random=~1|mtn, data=piar5.sum)
summary(piar5.baf.mod)
anova(piar5.baf.mod)
# P = .9659

# Linear model 170: BAF and mean resin duct density
piar5.baf.mod2 <- lme(duct.den.mean ~ BAF, random=~1|mtn, data=piar5.sum)
summary(piar5.baf.mod2)
anova(piar5.baf.mod2)
# P = .1139

# Linear model 171: BAF and mean ring width
piar5.baf.mod3 <- lme(ring.width.mean ~ BAF, random=~1|mtn, data=piar5.sum)
summary(piar5.baf.mod3)
anova(piar5.baf.mod3)
# P = .1131

# Plot linear model 171
ggplot(piar5.sum, aes(BAF, ring.width.mean, color=mtn)) +
  facet_grid(spcode ~ .,labeller = as_labeller(species_names_facet)) +
  theme(strip.text.y = element_text(size=5)) +
  geom_point() +
  labs(x= "Basal area factor",
       y= "Mean ring width")+
  scale_color_manual(name= "Mountain range",
                     labels = mountain_names,
                     values= mycolours)

# Linear model 172: BAF and mean basal area increment
piar5.baf.mod4 <- lme(bai.mean ~ BAF, random=~1|mtn, data=piar5.sum)
summary(piar5.baf.mod4)
anova(piar5.baf.mod4)
# P = .0438
# This makes sense because it should be very similar to ring width

## 2. Are resin duct and growth traits affected by tree age?

# Linear model 173: Tree age and mean resin duct density
piar5.age.mod1 <- lme(duct.den.mean ~ max.age, random=~1|mtn, data=piar5.sum)
summary(piar5.age.mod1)
anova(piar5.age.mod1)
# P= .3942

# Plot linear model 173:
ggplot(piar5.sum, aes(max.age, duct.den.mean)) +
  geom_point()

# Linear model 174: Tree age and mean resin duct count
piar5.age.mod2 <- lme(duct.count.mean ~ max.age, random=~1|mtn, data=piar5.sum)
summary(piar5.age.mod2)
anova(piar5.age.mod2)
# P = .5676

# Linear model 175: Tree age and ring width
piar5.age.mod3 <- lme(ring.width.mean ~ max.age, random=~1|mtn, data=piar5.sum)
summary(piar5.age.mod3)
anova(piar5.age.mod3)
# P = .5126

# Linear model 176: Tree age and basal area increment
piar5.age.mod4 <- lme(bai.mean ~ max.age, random=~1|mtn, data=piar5.sum)
summary(piar5.age.mod4)
anova(piar5.age.mod4)
# P = .0106

## 3. Does elevation have a significant effect on resin duct and growth traits?

# Linear model 177: Elevation and mean resin duct density
piar5.elev.mod1 <- lme(duct.den.mean ~ elev, random=~1|mtn, data=piar5.sum)
summary(piar5.elev.mod1)
anova(piar5.elev.mod1)
# p = .8612

# Linear model 178: Elevation and mean resin duct count
piar5.elev.mod2 <- lme(duct.count.mean ~ elev, random=~1|mtn, data=piar5.sum)
summary(piar5.elev.mod2)
anova(piar5.elev.mod2)
# P = .0081

# Linear model 179: Elevation and ring width
piar5.elev.mod3 <- lme(ring.width.mean ~ elev, random=~1|mtn, data=piar5.sum)
summary(piar5.elev.mod3)
anova(piar5.elev.mod3)
# P =.1520

# Linear model 180: Elevation and basal area increment
piar5.elev.mod4 <- lme(bai.mean ~ elev, random=~1|mtn, data=piar5.sum)
summary(piar5.elev.mod4)
anova(piar5.elev.mod4)
# P = .2973

## 4. Does radiation have an effect on resin duct and growth traits?

# Linear model 181: Radiation and mean resin duct density
piar5.rad.mod1 <- lme(duct.den.mean ~ radiation, random=~1|mtn, data=piar5.sum)
summary(piar5.rad.mod1)
anova(piar5.rad.mod1)
# p = .5046

# Linear model 182: Radiation and mean resin duct count
piar5.rad.mod2 <- lme(duct.count.mean ~ radiation, random=~1|mtn, data=piar5.sum)
summary(piar5.rad.mod2)
anova(piar5.rad.mod2)
# P = .5017

# Linear model 183: Radiation and ring width
piar5.rad.mod3 <- lme(ring.width.mean ~ radiation, random=~1|mtn, data=piar5.sum)
summary(piar5.rad.mod3)
anova(piar5.rad.mod3)
# P = .7976

# Linear model 184: Radiation and basal area increment
piar5.rad.mod4 <- lme(bai.mean ~ radiation, random=~1|mtn, data=piar5.sum)
summary(piar5.rad.mod4)
anova(piar5.rad.mod4)
# P = .6256

## 5. Does slope have a significant effect on resin duct and growth traits?

# Linear model 185: Slope and mean resin duct density
piar5.slope.mod1 <- lme(duct.den.mean ~ slope, random=~1|mtn, data=piar5.sum)
summary(piar5.slope.mod1)
anova(piar5.slope.mod1)
# p = .8474

# Linear model 186: Slope and mean resin duct count
piar5.slope.mod2 <- lme(duct.count.mean ~ slope, random=~1|mtn, data=piar5.sum)
summary(piar5.slope.mod2)
anova(piar5.slope.mod2)
# P = .0256

# Linear model 187: Slope and ring width
piar5.slope.mod3 <- lme(ring.width.mean ~ slope, random=~1|mtn, data=piar5.sum)
summary(piar5.slope.mod3)
anova(piar5.slope.mod3)
# P =.4004

# Linear model 188: Slope and basal area increment
piar5.slope.mod4 <- lme(bai.mean ~ slope, random=~1|mtn, data=piar5.sum)
summary(piar5.slope.mod4)
anova(piar5.slope.mod4)
# P = .4089

## 6. Does the downstream distance along flow path have an effect on
##    resin duct and/org growth characteristics?

# Linear model 189: Downstream distance and mean resin duct density
piar5.ldv2.mod1 <- lme(duct.den.mean ~ ldist_valley2, random=~1|mtn, data=piar5.sum)
summary(piar5.ldv2.mod1)
anova(piar5.ldv2.mod1)
# p = .7224

# Linear model 190: Downstream distance and mean resin duct count
piar5.ldv2.mod2 <- lme(duct.count.mean ~ ldist_valley2, random=~1|mtn, data=piar5.sum)
summary(piar5.ldv2.mod2)
anova(piar5.ldv2.mod2)
# P = .023

# Linear model 191: Downstream distance and ring width
piar5.ldv2.mod3 <- lme(ring.width.mean ~ ldist_valley2, random=~1|mtn, data=piar5.sum)
summary(piar5.ldv2.mod3)
anova(piar5.ldv2.mod3)
# P =.2095

# Linear model 192: Downstream distance and basal area increment
piar5.ldv2.mod4 <- lme(bai.mean ~ ldist_valley2, random=~1|mtn, data=piar5.sum)
summary(piar5.ldv2.mod4)
anova(piar5.ldv2.mod4)
# P = .3728
