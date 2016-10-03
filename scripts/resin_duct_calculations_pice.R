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
