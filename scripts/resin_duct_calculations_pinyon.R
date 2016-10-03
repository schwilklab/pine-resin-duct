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
