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