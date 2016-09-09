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

# Models explore relationship to BAI and resin duct density.

library(ggplot2)
library(lme4)
library(MuMIn)
library(afex) # for p values in lmer/glmer models. 
afex_options(method_mixed="LRT") #Bootstrapping can be slow, use hrothgar for that.

source("read_rings.R")


################### 1. BAI and individual species ###########################################


# random intercept
bai.mod.ri <- lmer(bai.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

# random slope and intercept
bai.mod.rsi <- lmer(bai.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                      (age_s|tag), data=mdata, REML=FALSE)

# test to see which AIC value is better
anova(bai.mod.ri, bai.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
bai.mod.rsi2 <- lmer(bai.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                       (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(bai.mod.rsi, bai.mod.rsi2)

# again, much better model.  We need random slopes for age and PMDI

bai.mod.rsi3 <- lmer(bai.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                       (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                     data=mdata, REML=FALSE)

anova(bai.mod.rsi2, bai.mod.rsi3)

# Include the calendar year aspect as random intercept

bai.mod.full <- mixed(bai.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                        (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                      data=mdata, REML=FALSE)

summary(bai.mod.full)
anova(bai.mod.full)

# Species and age have a signficant effect, as well as the interaction
# between species and age, competition (BAF), and drought.
# So let's drop non significant interaction terms and create a model with
# only significant interactions.

bai.mod.simple <- mixed(bai.log_s ~ spcode*(age_s + PMDI_3yrlag_s +BAF_s) +
                          (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                        data=mdata, REML=FALSE)

summary(bai.mod.simple)
anova(bai.mod.simple)
# lsmeans(bai.mod.simple, pairwise~spcode)



################### 2. BAI and grouped species ###########################################


# random intercept
cmn.bai.mod.ri <- lmer(bai.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                         (1|tag), data=mdata, REML=FALSE)

# random slope and intercept
cmn.bai.mod.rsi <- lmer(bai.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                          (age_s|tag), data=mdata, REML=FALSE)

# test to see which AIC value is better
anova(cmn.bai.mod.ri, cmn.bai.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.bai.mod.rsi2 <- lmer(bai.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                           (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(cmn.bai.mod.rsi, cmn.bai.mod.rsi2)

# again, much better model.  We need random slopes for age and PMDI

cmn.bai.mod.rsi3 <- lmer(bai.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                           (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                         data=mdata, REML=FALSE)

anova(cmn.bai.mod.rsi2, cmn.bai.mod.rsi3)

# Include the calendar year aspect as random intercept

cmn.bai.mod.full <- mixed(bai.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                            (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                          data=mdata, REML=FALSE)

summary(cmn.bai.mod.full)
anova(cmn.bai.mod.full)

# Species and age have a signficant effect, as well as the interaction
# between species and age, competition (BAF), and drought (possibly).
# So let's drop non significant interaction terms and create a model with
# only significant interactions.

cmn.bai.mod.simple <- mixed(bai.log_s ~ subsections*(age_s + PMDI_3yrlag_s +BAF_s) +
                              (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                            data=mdata, REML=FALSE)

summary(cmn.bai.mod.simple)
anova(cmn.bai.mod.simple)
# lsmeans(cmn.bai.mod.simple, pairwise~subsections)

# Same thing as listed above, Ponderosa>Strobiformis>Pinyon


################### 3. Resin Duct Density and individual species ###########################################


den.mod.ri <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                     (1|tag), data=mdata, REML=FALSE)

den.mod.rsi <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                      (age_s|tag), data=mdata, REML=FALSE)

anova(den.mod.ri, den.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
den.mod.rsi2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                       (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(den.mod.rsi, den.mod.rsi2)

# So no need to include random slope for PMDI

# try calendar.year random intercept:
den.mod.rsi3 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                       (age_s|tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(den.mod.rsi, den.mod.rsi3)
# Ok, so some evidence. I guess it is worth including and we get the degrees of
# freedom right that way

den.mod.full <- mixed(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                        (age_s | tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(den.mod.full)
anova(den.mod.full)

# There is a signficant effect of species and age on duct density.  Interaction
# between species and age and bai are significant as well.
# So let's drop non significant interaction terms

den.mod.simple <- mixed(duct.density.log_s ~ spcode*(age_s + bai.log_s) +
                          (age_s | tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(den.mod.simple)
anova(den.mod.simple)
# lsmeans(den.mod.simple, "spcode")

# Same thing as listed before.  Significant effect of species and age, also
# interaction between species and age, and bai and age.  
# PICE>PIED>PIAR5>PIPO>PIST3


################### 4. Resin Duct Density and grouped species ###########################################


cmn.den.mod.ri <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai.log_s) + mtn +
                         (1|tag), data=mdata, REML=FALSE)

cmn.den.mod.rsi <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                          (age_s|tag), data=mdata, REML=FALSE)

anova(cmn.den.mod.ri, cmn.den.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.den.mod.rsi2 <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +bai.log_s) + mtn +
                           (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)

anova(cmn.den.mod.rsi, cmn.den.mod.rsi2)

# Random slope is not better

# try calendar.year random intercept:
cmn.den.mod.rsi3 <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                           (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(cmn.den.mod.rsi, cmn.den.mod.rsi3)
# No evidence indicating that this is better.  The more basic model seems
# to be the better fit.

cmn.den.mod.full <- mixed(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + bai.log_s) + mtn +
                            (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.den.mod.full)
anova(cmn.den.mod.full)

# So let's drop non significant interaction terms
cmn.den.mod.simple <- mixed(duct.density.log_s ~ subsections*(age_s + elev_s + bai.log_s) + mtn +
                              (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.den.mod.simple)
anova(cmn.den.mod.simple)
# lsmeans(cmn.den.mod.simple, "subsections")

# Interesting that there is a signficant interaction between elevation and species in terms 
# of resin duct density that was not pulled out in individual species.  Also, mountain range
# has a significant effect when species are lumped together.  Pinyon pines have the highest 
# resin duct density, then Ponderosa, and then Strobiformis.





###################################################################
####  Run models using ring width instead of BAI.log values #######
###################################################################





################### 1. rw and individual species ###########################################


# random intercept
rw.mod.ri <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                    (1|tag), data=mdata, REML=FALSE)

# random slope and intercept
rw.mod.rsi <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                     (age_s|tag), data=mdata, REML=FALSE)

# test to see which AIC value is better
anova(rw.mod.ri, rw.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
rw.mod.rsi2 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                      (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(rw.mod.rsi, rw.mod.rsi2)

# again, much better model.  We need random slopes for age and PMDI

rw.mod.rsi3 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                      (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                    data=mdata, REML=FALSE)

anova(rw.mod.rsi2, rw.mod.rsi3)

# Include the calendar year aspect as random intercept

rw.mod.full <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                       (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                     data=mdata, REML=FALSE)

summary(rw.mod.full)
anova(rw.mod.full)



rw.mod.simple <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s +elev_s) +
                         (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                       data=mdata, REML=FALSE)

summary(rw.mod.simple)
anova(rw.mod.simple)
# lsmeans(rw.mod.simple, pairwise~spcode)

# PIST3>PIPO>PIED>PICE>PIAR5



################### 2. rw and grouped species ###########################################


# random intercept
cmn.rw.mod.ri <- lmer(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                        (1|tag), data=mdata, REML=FALSE)

# random slope and intercept
cmn.rw.mod.rsi <- lmer(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                         (age_s|tag), data=mdata, REML=FALSE)

# test to see which AIC value is better
anova(cmn.rw.mod.ri, cmn.rw.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.rw.mod.rsi2 <- lmer(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                          (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(cmn.rw.mod.rsi, cmn.rw.mod.rsi2)

# again, much better model.  We need random slopes for age and PMDI

cmn.rw.mod.rsi3 <- lmer(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                          (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                        data=mdata, REML=FALSE)

anova(cmn.rw.mod.rsi2, cmn.rw.mod.rsi3)

# Include the calendar year aspect as random intercept

cmn.rw.mod.full <- mixed(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                           (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                         data=mdata, REML=FALSE)

summary(cmn.rw.mod.full)
anova(cmn.rw.mod.full)



cmn.rw.mod.simple <- mixed(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s) +
                             (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                           data=mdata, REML=FALSE)

summary(cmn.rw.mod.simple)
anova(cmn.rw.mod.simple)
# lsmeans(cmn.rw.mod.simple, pairwise~subsections)

# Strobiformis>Ponderosa>Pinyon


################### 3. Resin Duct Density and individual species ###########################################


rwden.mod.ri <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                       (1|tag), data=mdata, REML=FALSE)

rwden.mod.rsi <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                        (age_s|tag), data=mdata, REML=FALSE)

anova(rwden.mod.ri, rwden.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
rwden.mod.rsi2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                         (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
anova(rwden.mod.rsi, rwden.mod.rsi2)

# So no need to include random slope for PMDI

# try calendar.year random intercept:
rwden.mod.rsi3 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                         (age_s|tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(rwden.mod.rsi, rwden.mod.rsi3)
# Ok, so some evidence. I guess it is worth including and we get the degrees of
# freedom right that way

rwden.mod.full <- mixed(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                          (age_s | tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(rwden.mod.full)
anova(rwden.mod.full)



rwden.mod.simple <- mixed(duct.density.log_s ~ spcode*(age_s + ring.width_s) +
                            (age_s | tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(rwden.mod.simple)
anova(rwden.mod.simple)
# lsmeans(rwden.mod.simple, "spcode")

# PICE>PIED>PIAR5>PIPO>PIST3


################### 4. Resin Duct Density and grouped species ###########################################


cmn.rwden.mod.ri <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                           (1|tag), data=mdata, REML=FALSE)

cmn.rwden.mod.rsi <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                            (age_s|tag), data=mdata, REML=FALSE)

anova(cmn.rwden.mod.ri, cmn.rwden.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cmn.rwden.mod.rsi2 <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                             (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)

anova(cmn.rwden.mod.rsi, cmn.rwden.mod.rsi2)

# Random slope is not better

# try calendar.year random intercept:
cmn.rwden.mod.rsi3 <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                             (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)

anova(cmn.rwden.mod.rsi, cmn.rwden.mod.rsi3)
# No evidence indicating that this is better.  The more basic model seems
# to be the better fit.

cmn.rwden.mod.full <- mixed(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                              (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.rwden.mod.full)
anova(cmn.rwden.mod.full)

# So let's drop non significant interaction terms
cmn.rwden.mod.simple <- mixed(duct.density.log_s ~ subsections*(age_s + elev_s + ring.width_s) +
                                (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.rwden.mod.simple)
anova(cmn.rwden.mod.simple)
# lsmeans(cmn.rwden.mod.simple, "subsections")

# Pinyon>Strobiformis>Ponderosa