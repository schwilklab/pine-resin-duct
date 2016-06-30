# mods.R

# plots and lme4 models

# Setting up models to determine the best model that explains what
# is happening with the data. Three different types of models are 
# produced.  Models looking at resin duct density, count, and bai.
# For each dependent variable analyzed, a full model is produced
# which includes all variables, as well as seperate models which
# include only the indvidual varaibles by themselves to get an 
# idea on the relative influence they have as a predictor. Lastly,
# a null model is produced to compare to.

library(ggplot2)
library(lme4)
library(MuMIn)
library(afex) # for p values in lmer/glmer models. 
afex_options(method_mixed="LRT") #Bootstrapping can be slow, use hrothgar for that.

source("read_rings.R")

# read data, only select complete cases so no NA's exist
mdata <- ring_data[complete.cases(ring_data), ] %>%
  filter(age!=1) %>% # remove the first year of growth since no resin
                              # ducts are present in pith remove last year of
                              # data since these are only partial growth years
  filter(calendar.year != 2015) 

# transforms
mdata <- mdata %>% mutate(duct.per.circ = resin.duct.count / 2*r2*pi,
                          duct.density.log = log(duct.density+1),
                          fyear = as.factor(calendar.year))
## Rescale numeric variables ##
mdata <- mdata %>% mutate_each(funs(s = scale(.)), -tag, -spcode, -mtn, -date, -fyear)



## Tree growth ##

## Graphical exploration ##

ggplot(mdata, aes(PMDI_3yrlag_s, ring.width_s, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

# little effect for PIAR5, but looks like positive effect for others

ggplot(mdata, aes(age_s, ring.width_s, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)
# A triangular relationship.  Hm, max width goes down with age. tricky.

# age and pmdi:
ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
               aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)


ggplot(mdata, aes(BAF_s, ring.width_s)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)
# weak negative?

ggplot(mutate(mdata, fpmdi=cut(PMDI_3yrlag, c(-4,-2,0,2,4))),
               aes(age_s, ring.width_s, color=fpmdi)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)
# Ok, interesting. we can see that PIAR5 behaves differently : no effec tof age
# or PMDI opn ring width. For others, there is effect of PMDI in correct
# direction and also age effect.

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
               aes(BAF, ring.width_s, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)




# NOTE: no indication of radiation or slope (not shown), so ommitting
growth.mod.ri <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (1|tag), data=mdata, REML=FALSE)

growth.mod.rsi <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s|tag), data=mdata, REML=FALSE)

anova(growth.mod.ri, growth.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
growth.mod.rsi2 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s+PMDI_3yrlag|tag), data=mdata, REML=FALSE)
anova(growth.mod.rsi, growth.mod.rsi2)
# again, much better model.  We need random slopes for age and PMDI

growth.mod.rsi3 <-  lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s+PMDI_3yrlag|tag) + (1 | calendar.year), data=mdata, REML=FALSE)


anova(growth.mod.rsi2, growth.mod.rsi3)
# again, better model. This is getting complicated!

# use afex::mixed() for p values:
growth.mod.full <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) 
                         (age_s+PMDI_3yrlag|tag) + (1 | calendar.year), data=mdata, REML=FALSE)

summary(growth.mod.full)
anova(growth.mod.full)

# So let's drop non significant interaction terms
growth.mod.simple <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s) + elev_s +
                             (age_s + PMDI_3yrlag | tag) + (1 | calendar.year),
                           data=mdata, REML=FALSE)

summary(growth.mod.simple)
anova(growth.mod.simple)
#lsmeans(growth.mod.simple, pairwise~spcode)

# So species differ in growth rate (PIST3 > PIPO > PIED > PICE > PIAR5), No
# main effect of age or PMDI, but there are interactions with species
# (investigate?). slight NEGATIVE effect of elevation on growth.


###### 1. Resin Duct Density ##########

## Graphical exploration ##

# overall effect of age:
ggplot(mdata, aes(age_s, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)

# elevation effect?
ggplot(mdata, aes(elev, duct.density.log)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)
# no

# radiation effect?
ggplot(mdata, aes(radiation_s, duct.density.log)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)
# no

ggplot(mdata, aes(calendar.year, duct.density.log)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)

ggplot(mutate(mdata, felev=cut(elev, c(1500,1700, 1900, 2100, 2300))),
       aes(age_s, duct.density.log, color=felev)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)




# effect of PMDI?
ggplot(mdata, aes(PMDI_3yrlag_s, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")
# NO.

ggplot(mutate(mdata, fpmdi=cut(PMDI_3yrlag, c(-4,-2,0,2,4))),
       aes(BAF_s, duct.density.log, color=fpmdi)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)


ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
               aes(BAF, duct.density.log, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)





# NOTE: no indication of radiation or slope (not shown), so ommitting
den.mod.ri <- lmer(duct.density.log ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (1|tag), data=mdata, REML=FALSE)

den.mod.rsi <- lmer(duct.density.log ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s|tag), data=mdata, REML=FALSE)

anova(den.mod.ri, den.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
den.mod.rsi2 <- lmer(duct.density.log ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s+PMDI_3yrlag|tag), data=mdata, REML=FALSE)
anova(den.mod.rsi, den.mod.rsi2)

# So no need to include random slope for PMDI

# try calendar.year random intercept:
den.mod.rsi3 <- lmer(duct.density.log ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s|tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(den.mod.rsi, den.mod.rsi3)
# Ok, so some evidence. I guess it is worth including and we get the degrees of
# freedom rioght that rway

den.mod.full <- mixed(duct.density.log ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s | tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(den.mod.full)
anova(den.mod.full)

# So let's drop non significant interaction terms
den.mod.simple <- mixed(duct.density.log ~ spcode*age_s +
                           (age_s | tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(den.mod.simple)
anova(den.mod.simple)
lsmeans(den.mod.simple, "spcode")

# So species differ in duct density: pinyon pines are greater than the rest.
# duct density goes down with age, but this efffect differs by species.




###### 2. Resin Duct Count ##########
# Use density conclusions as starting point


count.mod.full1 <- glmer(resin.duct.count ~  spcode*(ring.area_s + age_s)  + PMDI_3yrlag_s +
                          elev_s + 
                          (1 | tag),
                          data=mdata, family=poisson)

count.mod.full2 <- glmer(resin.duct.count ~  spcode*(ring.area_s + age_s)  + PMDI_3yrlag_s +
                           elev_s +
                           (1 + PMDI_3yrlag_s | tag),
                           data=mdata, family=poisson)

anova(count.mod.full1, count.mod.full2)
# second is better

## count.mod.full3 <- glmer(resin.duct.count ~  spcode*(ring.area_s + age_s)  + PMDI_3yrlag_s +
##                            elev_s +
##                            (age | tag) +
##                            (1 | calendar.year),
##                          data=mdata, family=poisson)

## anova(count.mod.full2, count.mod.full3)
# fails to converge.  I've tried many combinations





summary(count.mod.full2)
anova(count.mod.full2)


count.mod.full <- mixed(resin.duct.count ~  spcode*(ring.area_s + age_s)  + PMDI_3yrlag_s +
                        elev_s +
                        (PMDI_3yrlag_s | tag),
                        data=mdata, family=poisson)

# ack convergence failures on some models


# simpler:
count.mod.simple <- mixed(resin.duct.count ~  spcode*(ring.area_s + age_s) +
                        (1 + PMDI_3yrlag_s | tag),
                        data=mdata, family=poisson)

anova(count.mod.simple)


# ok, needs work

