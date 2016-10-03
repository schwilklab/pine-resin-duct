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