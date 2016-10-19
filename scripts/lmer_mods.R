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


# # random intercept
# cmn.rw.mod.ri <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s + ldist_valley2_s) + mtn +
#                         (1|tag), data=mdata, REML=FALSE)
# 
# # species as random intercept as well
# cmn.rw.mod.ri2 <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s + ldist_valley2_s) + mtn +
#                         (1|spcode/tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rw.mod.ri, cmn.rw.mod.ri2)
# 
# # It looks like AIC values indicate that it is best to not include species as a random
# # intercept.
# 
# # random slope and intercept
# cmn.rw.mod.rsi <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s + ldist_valley2_s) + mtn +
#                          (age_s|tag), data=mdata, REML=FALSE)
# 
# # test to see which AIC value is better
# anova(cmn.rw.mod.ri, cmn.rw.mod.rsi) # so random slope for age much better.
# 
# # try random slope for PMDI
# cmn.rw.mod.rsi2 <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s + ldist_valley2_s) + mtn +
#                           (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
# anova(cmn.rw.mod.rsi, cmn.rw.mod.rsi2)
# 
# # again, much better model.  We need random slopes for age and PMDI
# 
# cmn.rw.mod.rsi3 <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s + ldist_valley2_s) + mtn +
#                           (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
#                         data=mdata, REML=FALSE)
# 
# anova(cmn.rw.mod.rsi2, cmn.rw.mod.rsi3)

# Include the calendar year aspect as random intercept

cmn.rw.mod.full <- mixed(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BAF_s + elev_s) + mtn +
                         (age_s + PMDI_3yrlag_s|tag) + (1 | calendar.year), 
                         data=mdata, REML=FALSE)

summary(cmn.rw.mod.full)
anova(cmn.rw.mod.full)




cmn.rw.mod.simple <- mixed(ring.width_s ~ subsections* (age_s + PMDI_3yrlag_s + age_s:PMDI_3yrlag_s) +
                           (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                           data=mdata, REML=FALSE)

summary(cmn.rw.mod.simple)
anova(cmn.rw.mod.simple)

# Create new dataframe with coefficients from summary
coefs_ringwidth <- data.frame(coef(summary(cmn.rw.mod.simple)))
# use normal distribution to approximate p-value (not very conservative)
coefs_ringwidth$p.z <- 2 * (1 - pnorm(abs(coefs_ringwidth$t.value)))

# Add a new column with calculations with exact slope and intercept and not just relationship
coefs_ringwidth$Estimate_calc <- length(coefs_ringwidth$Estimate) # will change numbers

# select vector elements by position to do calculations

coefs_ringwidth$Estimate_calc[1] <- coefs_ringwidth$Estimate[1]
coefs_ringwidth$Estimate_calc[2] <- coefs_ringwidth$Estimate[1]+ coefs_ringwidth$Estimate[2]
coefs_ringwidth$Estimate_calc[3] <- coefs_ringwidth$Estimate[1]+ coefs_ringwidth$Estimate[3]
coefs_ringwidth$Estimate_calc[4] <- coefs_ringwidth$Estimate[4]
coefs_ringwidth$Estimate_calc[5] <- coefs_ringwidth$Estimate[5]
coefs_ringwidth$Estimate_calc[6] <- coefs_ringwidth$Estimate[6]
coefs_ringwidth$Estimate_calc[7] <- coefs_ringwidth$Estimate[4]+ coefs_ringwidth$Estimate[7]
coefs_ringwidth$Estimate_calc[8] <- coefs_ringwidth$Estimate[4]+ coefs_ringwidth$Estimate[8]
coefs_ringwidth$Estimate_calc[9] <- coefs_ringwidth$Estimate[5]+ coefs_ringwidth$Estimate[9]
coefs_ringwidth$Estimate_calc[10] <- coefs_ringwidth$Estimate[5]+ coefs_ringwidth$Estimate[10]
coefs_ringwidth$Estimate_calc[11] <- coefs_ringwidth$Estimate[6]+ coefs_ringwidth$Estimate[11]
coefs_ringwidth$Estimate_calc[12] <- coefs_ringwidth$Estimate[6]+ coefs_ringwidth$Estimate[12]


# Post-hoc testing

# Subsection and age
pairs(lstrends(cmn.rw.mod.simple, "subsections", var="age_s"))
# Subsection and drought
pairs(lstrends(cmn.rw.mod.simple, "subsections", var="PMDI_3yrlag_s"))

# Run the same model as lmer to check heteroscedasticity

# cmn.rw.mod.simple.lmer <- lmer(ring.width_s ~ subsections* (age_s + PMDI_3yrlag_s + age_s:PMDI_3yrlag_s) +
#                                (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
#                                data=mdata, REML=FALSE)

# plot(fitted(cmn.rw.mod.simple.lmer), residuals(cmn.rw.mod.simple.lmer),
#      col= "red")
# abline(h=0, lty=2)
# lines(smooth.spline(fitted(cmn.rw.mod.simple.lmer), residuals(cmn.rw.mod.simple.lmer)))



 
#### Results of model

## Subsections

# P= .0314974
# Strobus>Ponderosae>Cembroides
# There is a signficant difference between subsections.  Specifically, 
# Strobus is greater than Ponderosae which is greater than Cembroides.
#  It seems that there is a signficant difference between strous and ponderosae
# and strobus and cembroides. It doesn't look like there is a signficant difference
# between cembroides and ponderosae in terms of ring width.

## Drought (PMDI_3yrlag)

# P= .0001151
# This has a signgificant overall effect.  When looked at graphically, you
# can see that as drought conditions decrease (PMDI increases) ring widths
# increase.  Pretty strong relationship here.

## Subsections:age

# Strobus>Cembroides>Ponderosae
# P= 9.382e-05
# It looks like there is a general overall negative effect. Based on fixed
# effect estimates shown in the summary, it seems that there is a stronger negative
# effect with Strobus than cembroides and ponderosae.  Based on t values, it seems that
# there is a signficant difference between cembroides and ponderosae, as well as strobus
# and ponderosae.

## Subsections : Drought (Pmdi_3yrlag)
# Strobus>Cembroides>Ponderosae
# P= .0035446
# As mentioned earlier, there is a general overall positive effect that as
# PMDI values increase, ring width increases.  That effect is least seen with 
# Ponderosae, but I beleive that is due to Arizonica not being affected by drought
# drought values since it is located in a canyon area.  Based on t values it looks
# like there is a signficant differnce between cembroides and strobus, and ponderosae
# and strobus.  Strobus seems to be the most responsive to drought conditions.



################### 2. Resin Duct Density and subsections ###########################################


# cmn.rwden.mod.ri <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BAF_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                            (1|tag), data=mdata, REML=FALSE)
# 
# cmn.rwden.mod.ri2 <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BAF_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                            (1|spcode/tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.ri, cmn.rwden.mod.ri2)
# # AIC values indicate that the best model doesn't include species as random effect
# 
# cmn.rwden.mod.rsi <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BAF_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                             (age_s|tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.ri, cmn.rwden.mod.rsi) # so random slope for age much better.
# 
# # try random slope for PMDI
# cmn.rwden.mod.rsi2 <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BAF_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                              (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.rsi, cmn.rwden.mod.rsi2)
# 
# # Random slope is not better
# 
# # try calendar.year random intercept:
# cmn.rwden.mod.rsi3 <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BAF_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                              (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.rsi, cmn.rwden.mod.rsi3)
# # AIC values are the same, so I will use the model that specifies the random
# # effects more in order to specify correct degrees of freedom.

cmn.rwden.mod.full <- mixed(duct.density.log_s ~ subsections*((age_s * (PMDI_3yrlag_s + ring.width_s)) +
                            BAF_s + elev_s) + mtn +
                            (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                            data=mdata, REML=FALSE)

summary(cmn.rwden.mod.full)
anova(cmn.rwden.mod.full)


# So let's drop non significant interaction terms
cmn.rwden.mod.simple <- mixed(duct.density.log_s ~ subsections*(age_s + elev_s + ring.width_s + age_s:ring.width_s) +
                              (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                              data=mdata, REML=FALSE)

summary(cmn.rwden.mod.simple)
anova(cmn.rwden.mod.simple)

# Create new dataframe with coefficients from summary
coefs_ductdensity <- data.frame(coef(summary(cmn.rwden.mod.simple)))
# use normal distribution to approximate p-value (not very conservative)
coefs_ductdensity$p.z <- 2 * (1 - pnorm(abs(coefs_ductdensity$t.value)))

# Add a new column with calculations with exact slope and intercept and not just relationship
coefs_ductdensity$Estimate_calc <- length(coefs_ductdensity$Estimate) # will change numbers

# select vector elements by position to do calculations
coefs_ductdensity$Estimate_calc[1] <- coefs_ductdensity$Estimate[1]
coefs_ductdensity$Estimate_calc[2] <- coefs_ductdensity$Estimate[1]+ coefs_ductdensity$Estimate[2]
coefs_ductdensity$Estimate_calc[3] <- coefs_ductdensity$Estimate[1]+ coefs_ductdensity$Estimate[3]
coefs_ductdensity$Estimate_calc[4] <- coefs_ductdensity$Estimate[4]
coefs_ductdensity$Estimate_calc[5] <- coefs_ductdensity$Estimate[5]
coefs_ductdensity$Estimate_calc[6] <- coefs_ductdensity$Estimate[6]
coefs_ductdensity$Estimate_calc[7] <- coefs_ductdensity$Estimate[7]
coefs_ductdensity$Estimate_calc[8] <- coefs_ductdensity$Estimate[4]+ coefs_ductdensity$Estimate[8]
coefs_ductdensity$Estimate_calc[9] <- coefs_ductdensity$Estimate[4]+ coefs_ductdensity$Estimate[9]
coefs_ductdensity$Estimate_calc[10] <- coefs_ductdensity$Estimate[5]+ coefs_ductdensity$Estimate[10]
coefs_ductdensity$Estimate_calc[11] <- coefs_ductdensity$Estimate[5]+ coefs_ductdensity$Estimate[11]
coefs_ductdensity$Estimate_calc[12] <- coefs_ductdensity$Estimate[6]+ coefs_ductdensity$Estimate[12]
coefs_ductdensity$Estimate_calc[13] <- coefs_ductdensity$Estimate[6]+ coefs_ductdensity$Estimate[13]
coefs_ductdensity$Estimate_calc[14] <- coefs_ductdensity$Estimate[7]+ coefs_ductdensity$Estimate[14]
coefs_ductdensity$Estimate_calc[15] <- coefs_ductdensity$Estimate[7]+ coefs_ductdensity$Estimate[15]


# Post-hoc testing

# Subsections
lsmeans(cmn.rwden.mod.simple, pairwise~ subsections)
# Subsections and age
pairs(lstrends(cmn.rwden.mod.simple, "subsections", var="age_s"))
# Subsections and ring width
pairs(lstrends(cmn.rwden.mod.simple, "subsections", var="ring.width_s"))
# Subsections and elevation
pairs(lstrends(cmn.rwden.mod.simple, "subsections", var="elev_s"))


# # Create an lmer one to use to test heteroscedasticity

# cmn.rwden.mod.simple.lmer <- lmer(duct.density.log_s ~ subsections*(age_s + elev_s + ring.width_s + PMDI_3yrlag_s + subsections:age_s:ring.width_s) +
#                               subsections:age_s:PMDI_3yrlag_s + age_s:ring.width_s+
#                               (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
#                               data=mdata, REML=FALSE)
 
# plot(fitted(cmn.rwden.mod.simple.lmer), residuals(cmn.rwden.mod.simple.lmer),
#      col="red")
# abline(h=0, lty=2)
# lines(smooth.spline(fitted(cmn.rwden.mod.simple.lmer), residuals(cmn.rwden.mod.simple.lmer)))


# Cembroides>Strobus>Ponderosae
# So there is a signficant difference between subsections and age. There are
# aso siignficant interactions between subsections and age, subsections and elev,
# and subsections and ring width.  I'll go further into those relationships below.

## Subsections
# Cembroides>Strobus>Ponderosae
# P= <2.2e-16
# As previously thought from looking at the data, cembroides trees have a larger 
# resin duct density than the other two subsections.  It looks like there is a signficant
# difference between cembroides and ponderosae as well as cembroides an strobus, but
# not a difference between ponderosae and strobus 


## Age

# P= 1.771e-11
# It appears that resin duct density decreases with age when
# looking at an overall effect.


## Subsections:Age
# Intercept: Cembroides>Ponderosae>Strobus
# P= .004628
# Intitial intercept has cembroides greater than strobus and ponderosae. It appears
# that there is a signficnat difference between cembroides and ponderosae as well as 
# cembroides and strobus, but not between ponderosae and strobus.


## Subsections:Elevation
# Intercept: Cembroides>Strobus>Ponderosae
# P= .042082
# Initial intercept has cembroides greater than Ponderosae and Strobus. It looks
# like there is a signficant difference between cembroides and ponderosae as well as
# cembroides and strobus.  When the mean logged resin duct density per tree is graphed
# by elevation and seperated by subsection, it shows that cembroides' duct density
# increases with increased elevation where ponderosae and strobus decrease with elevation.


## Subsections:Ring.width
# Intercept: Strobus>Ponderosae>Cembroides
# P= 5.634e-09
# This one is a little complicated.  If you graph each tree's relationship with 
# ring width and duct density there isn't a clear relationship established.  When
# the mean ring widths are graphed with the corresponding mean resin duct density for 
# each tree, a more clear relationship comes out.  Cembroides resin duct density has
# a strong negative relationship with ring width.  Pondderosae is slightly negative
# and strobus is slightly positive.  If you look at how these relationships change
# in respect to age, and interesting relationship comes out.  There is a strong
# negative relationship with resin duct density and ring width for the first 10 years
# of a trees life.   After that, the relationship becomes positive, so the greater the
# ring width, the greater resin duct density.

