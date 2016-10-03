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