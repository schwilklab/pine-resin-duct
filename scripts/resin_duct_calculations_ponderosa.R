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