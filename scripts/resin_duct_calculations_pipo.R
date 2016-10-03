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