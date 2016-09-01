# lmer_mods.R

# plots and lme4 models

# Setting up models to determine the best model that explains what
# is happening with the data. Steps are as follows:
# 1. Look at all fixed variables when setting up full model.
# 2. Compare AIC values for regular, random intercept, and random slope.
#    Select model that has lowest AIC value.
# 3. Use afex::mixed() to get p values for the fixed effects in the model.
# 4. Drop non-significant effects based on results from mixed, and do new
#    model to include only significant effects.

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
mdata <- mdata %>% mutate(duct.per.circ = resin.duct.count / ((r2)^2*pi),
                          duct.density.log = log(duct.density+1),
                          fyear = as.factor(calendar.year))
## Rescale numeric variables ##
mdata <- mdata %>% mutate_each(funs(s = scale(.)), -tag, -spcode, -mtn, -date, -fyear, -cmn_name)


################### 1. BAI and individual species ###########################################



# NOTE: no indication of radiation or slope (not shown), so ommitting

# random intercept
bai.mod.ri <- lmer(bai_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

# random slope and intercept
bai.mod.rsi <- lmer(bai_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                      (age_s|tag), data=mdata, REML=FALSE)

# test to see which AIC value is better
anova(bai.mod.ri, bai.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
bai.mod.rsi2 <- lmer(bai_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                       (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(bai.mod.rsi, bai.mod.rsi2)

# again, much better model.  We need random slopes for age and PMDI

bai.mod.rsi3 <- lmer(bai_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                       (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                     data=mdata, REML=FALSE)

anova(bai.mod.rsi2, bai.mod.rsi3)

# Include the calendar year aspect as random intercept

bai.mod.full <- mixed(bai_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                      (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                      data=mdata, REML=FALSE)

summary(bai.mod.full)
anova(bai.mod.full)

# Species and age have a signficant effect, as well as the interaction
# between species and age, competition (BAF), and drought (possibly).
# So let's drop non significant interaction terms and create a model with
# only significant interactions.

bai.mod.simple <- mixed(bai_s ~ spcode*(age_s + PMDI_3yrlag_s +BAF_s) +
                          (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                        data=mdata, REML=FALSE)

summary(bai.mod.simple)
anova(bai.mod.simple)
# lsmeans(bai.mod.simple, pairwise~spcode)

# Species, age, species & age, species & competition, and species & drought
# are signifcant.  The interaction between species and drought is right at .05, 
# so there's a possibility that it isn't truly signficant, and it is only coming
# out as significant because of how the model is set up. BAI values by species are
# alligned as such: (PIPO>PIST3>PIED>PICE>PIAR5).  How the other variables influence
# BAI will be listed below (note: all are int relation to PIAR5).

# PICE: age= negative, drought= negative, competition= negative
# PIED: age= positive, drought= positive, competition= positive
# PIPO: age= positive, drought= positive, competition= negative
# PIST3: age= positive, drought= positive, competition= negative


################### 2. BAI and lumped species ###########################################



cmn.bai.mod.ri <- lmer(bai_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

cmn.bai.mod.rsi <- lmer(bai_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                      (age_s|tag), data=mdata, REML=FALSE)

anova(cmn.bai.mod.ri, cmn.bai.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.bai.mod.rsi2 <- lmer(bai_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                       (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(cmn.bai.mod.rsi, cmn.bai.mod.rsi2)

# again, much better model.  We need random slopes for age and PMDI

cmn.bai.mod.rsi3 <- lmer(bai_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                        (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                        data=mdata, REML=FALSE)

anova(cmn.bai.mod.rsi2, cmn.bai.mod.rsi3)

# Include the calendar year aspect as random intercept

cmn.bai.mod.full <- mixed(bai_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                         (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                         data=mdata, REML=FALSE)

summary(cmn.bai.mod.full)
anova(cmn.bai.mod.full)

# There is a significant difference between the lumped together species, age,
# drought, and possibly mountain range (.059 p value).  There's also 
# signficant interactions between lumped species and age, possibly drought (.051),
# and competition (BAF).
# Removing non-significant interaction terms to create simple model.

cmn.bai.mod.simple <- mixed(bai_s ~ cmn_name*(age_s + PMDI_3yrlag_s +BAF_s) + mtn +
                           (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                           data=mdata, REML=FALSE)

summary(cmn.bai.mod.simple)
anova(cmn.bai.mod.simple)
# lsmeans(cmn.bai.mod.simple, pairwise~cmn_name)

# Same variables come out as being significant as they did in the full model.
# BAI values are alligned as such (Ponderosa> Strobiformis > Pinyon). 
# How the other variables influence BAI will be listed below
# (note: all are in relation to Pinyon).

# Ponderosa: age= positive, drought= positive, competition= negative
# Strobiformis: age= positive, drought= positive, competition= negative




############### 3. Scaled Resin Duct Density with individual species  #####################

# Looking at scaled values rather than logged values

# NOTE: no indication of radiation or slope (not shown), so ommitting
den.mod.ri <- lmer(duct.density_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

den.mod.rsi <- lmer(duct.density_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                      (age_s|tag), data=mdata, REML=FALSE)

anova(den.mod.ri, den.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
den.mod.rsi2 <- lmer(duct.density_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                       (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(den.mod.rsi, den.mod.rsi2)

# Random slope is better

# try calendar.year random intercept:
den.mod.rsi3 <- lmer(duct.density_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                       (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(den.mod.rsi2, den.mod.rsi3)
# Strong evidence that adding calendar year as a random effect is better

den.mod.full <- mixed(duct.density_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                      (age_s+PMDI_3yrlag_s | tag) + (1|calendar.year),
                      data=mdata, REML=FALSE)

summary(den.mod.full)
anova(den.mod.full)

# Individual species, age, and basal area increment significantly influence
# annual resin duct density.  Also, there are interaction effects with species
# and age, drought, and bai.  Next step is to drop non significant interaction terms.

den.mod.simple <- mixed(duct.density_s ~ spcode*(age_s + PMDI_3yrlag_s + bai_s) +
                        (age_s+PMDI_3yrlag_s | tag) + (1|calendar.year),
                        data=mdata, REML=FALSE)

summary(den.mod.simple)
anova(den.mod.simple)
# lsmeans(den.mod.simple, "spcode")

# The same traits that were signficant in the full model were significant in this model.  
# It's interesting that drought by itself has about as minimal of an effect possible,
# but as an interaction with species, it becomes significant.  I don't know if the our 
# model is seeing drought values with the degrees of freedom equivilant to that of calendar 
# year, even with the random effects specified in the model, but based on previous conversation,
# that should be ok.  Resin duct density values in order are: (PIPO>PICE>PIED>PIAR5>PIST3) based
# on values calculated using lsmeans.  


############### 4. Scaled Resin Duct Density with lumped species  #####################

# Now run the same code with individual species being lumped
# together with closely related species


cmn.den.mod.ri <- lmer(duct.density_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

cmn.den.mod.rsi <- lmer(duct.density_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                      (age_s|tag), data=mdata, REML=FALSE)

anova(cmn.den.mod.ri, cmn.den.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.den.mod.rsi2 <- lmer(duct.density_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                       (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(cmn.den.mod.rsi, cmn.den.mod.rsi2)

# Random slope is better

# try calendar.year random intercept:
cmn.den.mod.rsi3 <- lmer(duct.density_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                         (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                         data=mdata, REML=FALSE)

anova(cmn.den.mod.rsi2, cmn.den.mod.rsi3)
# Strong evidence that adding calendar year as a random effect is better

cmn.den.mod.full <- mixed(duct.density_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                          (age_s+PMDI_3yrlag_s | tag) + (1|calendar.year),
                          data=mdata, REML=FALSE)

summary(cmn.den.mod.full)
anova(cmn.den.mod.full)

# So "species" has a signficant effect, age, basal area increment, and it looks like drought
# might have an effect (.079) on resin duct density.  Also, interactions between "species" 
# and age, bai, and drought (maybe, .08) have a significant effect.
# So let's drop non significant interaction terms.

cmn.den.mod.simple <- mixed(duct.density_s ~ cmn_name*(age_s + PMDI_3yrlag_s + bai_s) +
                            (age_s+PMDI_3yrlag_s | tag) + (1|calendar.year),
                            data=mdata, REML=FALSE)

summary(cmn.den.mod.simple)
anova(cmn.den.mod.simple)
#lsmeans(cmn.den.mod.simple, "cmn_name")

# The same traits that were significant were signficant in the simple model with 
# the p value for drought andinteraction between species and drought decreasing more,
# but still not under .05.  Based on means calculated by lsmeans, (Pinyon>Ponderosa>Strobiformis)
# in terms of resin duct density.


############### 5. Scaled Logged Resin Duct Density with lumped species  #####################


# NOTE: no indication of radiation or slope (not shown), so ommitting
logden.mod.ri <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

logden.mod.rsi <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                    (age_s|tag), data=mdata, REML=FALSE)

anova(logden.mod.ri, logden.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
logden.mod.rsi2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                        (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(logden.mod.rsi, logden.mod.rsi2)

# So no need to include random slope for PMDI

# try calendar.year random intercept:
logden.mod.rsi3 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                        (age_s|tag) + (1 | calendar.year),
                        data=mdata, REML=FALSE)

anova(logden.mod.rsi, logden.mod.rsi3)
# Ok, so some evidence. I guess it is worth including and we get the degrees of
# freedom right that way

logden.mod.full <- mixed(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                      (age_s | tag) + (1|calendar.year),
                      data=mdata, REML=FALSE)

summary(logden.mod.full)
anova(logden.mod.full)

# There is a signficant effect of species and age on dut density.  Interaction
# between sepcies and age and bai are significant as well.
# So let's drop non significant interaction terms

logden.mod.simple <- mixed(duct.density.log_s ~ spcode*(age_s + bai_s) +
                           (age_s | tag) + (1|calendar.year),
                           data=mdata, REML=FALSE)

summary(logden.mod.simple)
anova(logden.mod.simple)
# lsmeans(logden.mod.simple, "spcode")

# So species differ in duct density: pinyon pines are greater than the rest.
# duct density goes down with age, but this effect differs by species.




############### 6. Scaled Logged Resin Duct Density with lumped species  #####################


# Now run models using logged values rather than scaled values


cmn.logden.mod.ri <- lmer(duct.density.log_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                         (1|tag), data=mdata, REML=FALSE)

cmn.logden.mod.rsi <- lmer(duct.density.log_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                          (age_s|tag), data=mdata, REML=FALSE)

anova(cmn.logden.mod.ri, cmn.logden.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.logden.mod.rsi2 <- lmer(duct.density.log_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai_s) + mtn +
                           (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(cmn.logden.mod.rsi, cmn.logden.mod.rsi2)

# Random slope is not better

# try calendar.year random intercept:
cmn.logden.mod.rsi3 <- lmer(duct.density.log_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                           (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(cmn.logden.mod.rsi, cmn.logden.mod.rsi3)
# No evidence indicating that this is better.  The more basic model seems
# to be the better fit.

cmn.logden.mod.full <- mixed(duct.density.log_s ~ cmn_name*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai_s) + mtn +
                               (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.logden.mod.full)
anova(cmn.logden.mod.full)

# So let's drop non significant interaction terms
cmn.logden.mod.simple <- mixed(duct.density.log_s ~ cmn_name*(age_s + elev_s + bai_s) + mtn +
                                 (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.logden.mod.simple)
anova(cmn.logden.mod.simple)
# lsmeans(cmn.logden.mod.simple, "cmn_name")

# Interesting that there is a signficant interaction between elevation and species in terms 
# of resin duct density that was not pulled out in individual species.  Also, mountain range
# has a signficant effect when species are lumped together.  Pinyon pines have the highest 
# resin duct density.