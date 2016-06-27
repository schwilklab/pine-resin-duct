# mods.R

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
                          duct.density.log = log(duct.density+1))
## Rescale numeric variables ##
mdata <- mdata %>% mutate_each(funs(s = scale(.)), -tag, -spcode, -mtn, -date)


## Graphical exploration ##
ggplot(mdata, aes(age, duct.per.circ, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

ggplot(mdata, aes(age, duct.density, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm")

ggplot(mdata, aes(PMDI_3yrlag, log(duct.density+0.000001), group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)

ggplot(mutate(mdata, fpmdi=cut(PMDI_3yrlag, c(-4,-2,0,2,4)) ),
       aes(age, duct.density, group=tag)) +
  geom_point() +
  facet_grid(fpmdi  ~ spcode) +
  geom_smooth(method="lm", se=FALSE)

## Tree growth ##

## Graphical exploration ##
ggplot(mdata, aes(age, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ spcode) +
  geom_smooth(method="lm", se=FALSE)

ggplot(mdata, aes(BAF, ring.width, group=tag)) +
  geom_point() +
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
# use afex::mixed() for p values:
growth.mod.full <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s+PMDI_3yrlag|tag), data=mdata, REML=FALSE)

summary(growth.mod.full)
anova(growth.mod.full)

# So let's drop non significant interaction terms
growth.mod.simple <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s) + elev_s +
                           (age_s + PMDI_3yrlag | tag), data=mdata, REML=FALSE)

summary(growth.mod.simple)
anova(growth.mod.simple)
#lsmeans(growth.mod.simple, pairwise~spcode)

# So species differ in growth rate (PIST3 > PIPO > PIED > PICE > PIAR5), No
# main effect of age or PMDI, but there are interactions with species
# (investigate?). slight NEGATIVE effect of elevation on growth.


###### 1. Resin Duct Density ##########

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

den.mod.full <- mixed(duct.density.log ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
                         (age_s | tag), data=mdata, REML=FALSE)

summary(den.mod.full)
anova(den.mod.full)

# So let's drop non significant interaction terms
den.mod.simple <- mixed(duct.density.log ~ spcode*age_s + PMDI_3yrlag_s +
                           (age_s | tag), data=mdata, REML=FALSE)

summary(den.mod.simple)
anova(den.mod.simple)
lsmeans(den.mod.simple, "spcode")

# So species differ in duct density: pinyon pines are greater than the rest.
# duct density goes down with age, but this efffect differs by species. And
# duct density goes DOWN with PMDI! (lower duct density in good years) Could be
# effect of growth, yet now growth on PMDI overall effect.




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

## count.mod.full3 <- glmer(resin.duct.count ~  spcode*(ring.area_s + age_s)  + PMDI_3yrlag_s +
##                            elev_s +
##                            (age_s | tag),
##                          data=mdata, family=poisson)

# fails to converge

anova(count.mod.full1, count.mod.full2)

summary(count.mod.full2)
anova(count.mod.full2)


count.mod.full <- mixed(resin.duct.count ~  spcode*(ring.area_s + age_s)  + PMDI_3yrlag_s +
                        elev_s +
                        (1 + PMDI_3yrlag_s | tag),
                        data=mdata, family=poisson)

# ack convergence failures on some models


# simpler:
count.mod.simple <- mixed(resin.duct.count ~  spcode*(ring.area_s + age_s) +
                        (1 + PMDI_3yrlag_s | tag),
                        data=mdata, family=poisson)

anova(count.mod.simple)


# ok, needs work

