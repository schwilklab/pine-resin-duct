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


################### 1. rw and grouped species ###########################################


# random intercept
cmn.rw.mod.ri <- lmer(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                        (1|tag), data=mdata, REML=FALSE)

# species as random intercept as well
cmn.rw.mod.ri2 <- lmer(ring.width_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                        (1|spcode/tag), data=mdata, REML=FALSE)

anova(cmn.rw.mod.ri, cmn.rw.mod.ri2)

# It looks like AIC values indicate that it is best to not include species as a random
# intercept.

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


################### 2. Resin Duct Density and grouped species ###########################################


cmn.rwden.mod.ri <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                           (1|tag), data=mdata, REML=FALSE)

cmn.rwden.mod.ri2 <- lmer(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                           (1|spcode/tag), data=mdata, REML=FALSE)

anova(cmn.rwden.mod.ri, cmn.rwden.mod.ri2)
# AIC values indicate that the best model doesn't include species as random effect

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
# AIC values are the same, but BIC values are better for more simple model.  I'll run it
# with both examples.

cmn.rwden.mod.full <- mixed(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                              (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.rwden.mod.full)
anova(cmn.rwden.mod.full)

cmn.rwden.mod.full2 <- mixed(duct.density.log_s ~ subsections*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                              (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)

summary(cmn.rwden.mod.full2)
anova(cmn.rwden.mod.full2)

anova(cmn.rwden.mod.full, cmn.rwden.mod.full2)
# SO both of them have the same AIC values, but the one with the less random effects 
# indicates a lower BIC value.  In both cases, the same variables come out as being
# signficant with a small difference in value.  I think a case could be made for 
# either, and it just matters which one is the best to present in a paper, because they
# both say the same thing.

# So let's drop non significant interaction terms
cmn.rwden.mod.simple <- mixed(duct.density.log_s ~ subsections*(age_s + elev_s + ring.width_s) +
                                (age_s|tag), data=mdata, REML=FALSE)

summary(cmn.rwden.mod.simple)
anova(cmn.rwden.mod.simple)
# lsmeans(cmn.rwden.mod.simple, "subsections")

# Pinyon>Strobiformis>Ponderosa





##########################################################################
##########################################################################
# Run models using subsets of the data based on subsections

# I have to create the seperrate dataframes rather than making a call
# in the model because the scaled values in mdata are based on
# all trees, so errors occur in the model.  Create two seperate
# dataframes with ponderosae and cembroides.

# read data, only select complete cases so no NA's exist
ponderosae_data <- ring_data[complete.cases(ring_data), ] %>%
  filter(age!=1) %>% # remove the first year of growth since no resin
  # ducts are present in pith remove last year of
  # data since these are only partial growth years
  filter(calendar.year != 2015)  %>%
  filter(subsections=="Ponderosae") %>%
  mutate(duct.per.circ = resin.duct.count / ((r2)^2*pi),
         duct.density.log = log(duct.density+1),
         bai.log = log(bai+1),
         fyear = as.factor(calendar.year)) %>%
  mutate_each(funs(s = zscore(.)), -tag, -spcode, -mtn, -date, 
              -fyear, -subsections, -species_names)


cembroides_data <- ring_data[complete.cases(ring_data), ] %>%
  filter(age!=1) %>% # remove the first year of growth since no resin
  # ducts are present in pith remove last year of
  # data since these are only partial growth years
  filter(calendar.year != 2015)  %>%
  filter(subsections=="Cembroides") %>%
  mutate(duct.per.circ = resin.duct.count / ((r2)^2*pi),
         duct.density.log = log(duct.density+1),
         bai.log = log(bai+1),
         fyear = as.factor(calendar.year)) %>%
  mutate_each(funs(s = zscore(.)), -tag, -spcode, -mtn, -date, 
              -fyear, -subsections, -species_names)



################### 1. rw and ponderosae values ###########################################


# random intercept
pond.rw.mod.ri <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                         (1|tag), data=ponderosae_data, REML=FALSE)

#two random intercepts (tag within species)
pond.rw.mod.ri2 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                          (1|spcode/tag), data=ponderosae_data , REML=FALSE)

anova(pond.rw.mod.ri, pond.rw.mod.ri2)
# Species as a random effect produces a model that has a greater aic value, 
# I don't know if this is the best route to take.

# random slope and intercept
pond.rw.mod.rsi <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                          (age_s|tag), data=ponderosae_data, REML=FALSE)

# test to see which AIC value is better
anova(pond.rw.mod.ri, pond.rw.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
pond.rw.mod.rsi2 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                           (age_s+PMDI_3yrlag_s|tag), data=ponderosae_data, REML=FALSE)

anova(pond.rw.mod.rsi, pond.rw.mod.rsi2)

# Second model is better model

pond.rw.mod.rsi3 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                           (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                         data=ponderosae_data, REML=FALSE)

anova(pond.rw.mod.rsi2, pond.rw.mod.rsi3)

# Last model specifying random effects is best

pond.rw.mod.full <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                            (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                          data=ponderosae_data, REML=FALSE)

summary(pond.rw.mod.full)
anova(pond.rw.mod.full)



pond.rw.mod.simple <- mixed(ring.width_s ~ spcode*(PMDI_3yrlag_s + elev_s +  BAF_s) +
                              (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                            data=ponderosae_data, REML=FALSE)

summary(pond.rw.mod.simple)
anova(pond.rw.mod.simple)

# Ok, this is weird.  Now the correct degrees of freedom are being specified for
# spcode and it is coming out as signficant.  Interaction between pmdi and spcode
# comes out signficant as well with elevation at .07 and spcode:elev at .09.
# Not entirely sure what is happening here.



################### 2. Resin Duct Density and ponderosae values ###########################################


pond.rwden.mod.ri <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                            (1|tag), data=ponderosae_data, REML=FALSE)

pond.rwden.mod.ri2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                             (1|spcode/tag), data=ponderosae_data, REML=FALSE)

anova(pond.rwden.mod.ri, pond.rwden.mod.ri2)

# Again, including species as a random effect doesn't seem to be the best
# thing to do based on AIC values

pond.rwden.mod.rsi <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                             (age_s|tag), data=ponderosae_data, REML=FALSE)

anova(pond.rwden.mod.ri, pond.rwden.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
pond.rwden.mod.rsi2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                              (age_s+PMDI_3yrlag_s|tag), data=ponderosae_data, REML=FALSE)

anova(pond.rwden.mod.rsi, pond.rwden.mod.rsi2)

# Random slope is not better

# try calendar.year random intercept:
pond.rwden.mod.rsi3 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                              (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=ponderosae_data, REML=FALSE)

anova(pond.rwden.mod.rsi, pond.rwden.mod.rsi3)
# There is little evidence that this is more effective based on AIC values (difference of 1)
# but I will include it since it specifies the correct degrees of freedom

pond.rwden.mod.full <- mixed(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                               (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                             data=ponderosae_data, REML=FALSE)

# I receive an error messsage when I run this.
#Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  unable to evaluate scaled gradient
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
# 3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  unable to evaluate scaled gradient
# 4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

#### Can't run models ###


#summary(pond.rwden.mod.full)
#anova(pond.rwden.mod.full)





################### 2. rw and ponderosae values ###########################################


# random intercept
cemb.rw.mod.ri <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                         (1|tag), data=cembroides_data, REML=FALSE)

#two random intercepts (tag within species)
cemb.rw.mod.ri2 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                          (1|spcode/tag), data=cembroides_data , REML=FALSE)

anova(cemb.rw.mod.ri, cemb.rw.mod.ri2)
# Species as a random effect produces a model that has a greater aic value, 
# I don't know if this is the best route to take.

# random slope and intercept
cemb.rw.mod.rsi <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                          (age_s|tag), data=cembroides_data, REML=FALSE)

# test to see which AIC value is better
anova(cemb.rw.mod.ri, cemb.rw.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cemb.rw.mod.rsi2 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn  +
                           (age_s+PMDI_3yrlag_s|tag), data=cembroides_data, REML=FALSE)

anova(cemb.rw.mod.rsi, cemb.rw.mod.rsi2)

# Second model is better

cemb.rw.mod.rsi3 <- lmer(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                           (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
                         data=cembroides_data, REML=FALSE)

anova(cemb.rw.mod.rsi2, cemb.rw.mod.rsi3)

# Again, secod model is better, this is what I will use

cemb.rw.mod.full <- mixed(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) + mtn +
                            (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                          data=cembroides_data, REML=FALSE)

summary(cemb.rw.mod.full)
anova(cemb.rw.mod.full)



cemb.rw.mod.simple <- mixed(ring.width_s ~ spcode*(PMDI_3yrlag_s + elev_s +  BAF_s) +
                              (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                            data=cembroides_data, REML=FALSE)

summary(cemb.rw.mod.simple)
anova(cemb.rw.mod.simple)

# Ok, this is weird.  Now the correct degrees of freedom are being specified for
# spcode and it is coming out as signficant.  PMDI and elevation are also coming
# out as significant. Interaction between pmdi and spcode
# comes out signficant as well with elevation and spcode:BAF at .07.
# Not entirely sure what is happening here.
# lsmeans(cemb.rw.mod.simple, pairwise~subsections)

# Strobiformis>Ponderosa>Pinyon


################### 4. Resin Duct Density and ponderosae values ###########################################


cemb.rwden.mod.ri <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                            (1|tag), data=cembroides_data, REML=FALSE)

cemb.rwden.mod.ri2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                             (1|spcode/tag), data=cembroides_data, REML=FALSE)

anova(cemb.rwden.mod.ri, cemb.rwden.mod.ri2)

# Again, including species as a random effect doesn't seem to be the best
# thing to do based on AIC values

cemb.rwden.mod.rsi <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                             (age_s|tag), data=cembroides_data, REML=FALSE)

anova(cemb.rwden.mod.ri, cemb.rwden.mod.rsi) # so random slope for age much better.

# try random slope for PMDI
cemb.rwden.mod.rsi2 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s +ring.width_s) + mtn +
                              (age_s+PMDI_3yrlag_s|tag), data=cembroides_data, REML=FALSE)

anova(cemb.rwden.mod.rsi, cemb.rwden.mod.rsi2)

# Random slope is not better

# try calendar.year random intercept:
cemb.rwden.mod.rsi3 <- lmer(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                              (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=cembroides_data, REML=FALSE)

anova(cemb.rwden.mod.rsi, cemb.rwden.mod.rsi3)
# There is evidence that this model is better, this is what I will use.

cemb.rwden.mod.full <- mixed(duct.density.log_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s + ring.width_s) + mtn +
                               (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=cembroides_data, REML=FALSE)

summary(cemb.rwden.mod.full)
anova(cemb.rwden.mod.full)

cemb.rwden.mod.simple <- mixed(duct.density.log_s ~ spcode*(age_s + elev_s + ring.width_s) +
                                 (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=cembroides_data, REML=FALSE)

summary(cemb.rwden.mod.simple)
anova(cemb.rwden.mod.simple)

# Again, it's weird that the correct degrees of freedom are now specified with spcode.
# Spcode is signficant, so is elevation.  Also interaction between species, elev, and
# ring width is signficant.