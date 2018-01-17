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
library(xtable)

source("read_all.R")


################### 1. Ring Width ###########################################

# # random intercept
# cmn.rw.mod.ri <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BA_s + elev_s + ldist_valley2_s) + mtn +
#                         (1|tag), data=mdata, REML=FALSE)
# 
# # species as random intercept as well
# cmn.rw.mod.ri2 <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BA_s + elev_s + ldist_valley2_s) + mtn +
#                         (1|spcode/tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rw.mod.ri, cmn.rw.mod.ri2)
# 
# # It looks like AIC values indicate that it is best to not include species as a random
# # intercept.
# 
# # random slope and intercept
# cmn.rw.mod.rsi <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BA_s + elev_s + ldist_valley2_s) + mtn +
#                          (age_s|tag), data=mdata, REML=FALSE)
# 
# # test to see which AIC value is better
# anova(cmn.rw.mod.ri, cmn.rw.mod.rsi) # so random slope for age much better.
# 
# # try random slope for PMDI
# cmn.rw.mod.rsi2 <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BA_s + elev_s + ldist_valley2_s) + mtn +
#                           (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
# anova(cmn.rw.mod.rsi, cmn.rw.mod.rsi2)
# 
# # again, much better model.  We need random slopes for age and PMDI
# 
# cmn.rw.mod.rsi3 <- lmer(ring.width_s ~ subsections*((age_s * PMDI_3yrlag_s) + BA_s + elev_s + ldist_valley2_s) + mtn +
#                           (age_s+PMDI_3yrlag_s|tag) + (1 | calendar.year),
#                         data=mdata, REML=FALSE)
# 
# anova(cmn.rw.mod.rsi2, cmn.rw.mod.rsi3)

# Include the calendar year aspect as random intercept


# Include the calendar year as random intercept

cmn.rw.mod.full <- mixed(ring_width_detrended ~ subsection*(PMDI_3yrlag_s + BA_s + elev_s) +
                         (PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                         data=mdata, REML=FALSE)

summary(cmn.rw.mod.full)
anova(cmn.rw.mod.full)


## cmn.rw.mod.simple <- mixed(ring_width_detrended ~ subsection * PMDI_3yrlag_s + subsection*BA_s +
##                            (PMDI_3yrlag_s | tag) + (1 | calendar.year), 
##                            data=mdata, REML=FALSE)

cmn.rw.mod.simple <- mixed(ring_width_detrended ~ subsection + PMDI_3yrlag_s + subsection*BA_s +
                           (PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                           data=mdata, REML=FALSE)

summary(cmn.rw.mod.simple)
anova(cmn.rw.mod.simple)

print(xtable(anova(cmn.rw.mod.simple)))

#anova(cmn.rw.mod.simple, cmn.rw.mod.full)

## # Create new dataframe with coefficients from summary
## coefs_ringwidth <- data.frame(coef(summary(cmn.rw.mod.simple)))
## # use normal distribution to approximate p-value (not very conservative)
## coefs_ringwidth$p.z <- 2 * (1 - pnorm(abs(coefs_ringwidth$t.value)))

## # Add a new column with calculations with exact slope and intercept and not just relationship
## coefs_ringwidth$Estimate_calc <- length(coefs_ringwidth$Estimate) # will change numbers

## # select vector elements by position to do calculations

## coefs_ringwidth$Estimate_calc[1] <- coefs_ringwidth$Estimate[1]
## coefs_ringwidth$Estimate_calc[2] <- coefs_ringwidth$Estimate[1]+ coefs_ringwidth$Estimate[2]
## coefs_ringwidth$Estimate_calc[3] <- coefs_ringwidth$Estimate[1]+ coefs_ringwidth$Estimate[3]
## coefs_ringwidth$Estimate_calc[4] <- coefs_ringwidth$Estimate[4]
## coefs_ringwidth$Estimate_calc[5] <- coefs_ringwidth$Estimate[5]
## coefs_ringwidth$Estimate_calc[6] <- coefs_ringwidth$Estimate[6]
## coefs_ringwidth$Estimate_calc[7] <- coefs_ringwidth$Estimate[4]+ coefs_ringwidth$Estimate[7]
## coefs_ringwidth$Estimate_calc[8] <- coefs_ringwidth$Estimate[4]+ coefs_ringwidth$Estimate[8]
## coefs_ringwidth$Estimate_calc[9] <- coefs_ringwidth$Estimate[5]+ coefs_ringwidth$Estimate[9]
## coefs_ringwidth$Estimate_calc[10] <- coefs_ringwidth$Estimate[5]+ coefs_ringwidth$Estimate[10]
## coefs_ringwidth$Estimate_calc[11] <- coefs_ringwidth$Estimate[6]+ coefs_ringwidth$Estimate[11]
## coefs_ringwidth$Estimate_calc[12] <- coefs_ringwidth$Estimate[6]+ coefs_ringwidth$Estimate[12]


# Post-hoc testing

# Subsections
lsmeans(cmn.rw.mod.simple, pairwise~ subsection)

# Subsection and age
pairs(lstrends(cmn.rw.mod.simple, "subsection", var="BA_s"))
# Subsection and drought
pairs(lstrends(cmn.rw.mod.simple, "subsection", var="PMDI_3yrlag_s"))

# Run the same model as lmer to check heteroscedasticity

# cmn.rw.mod.simple.lmer <- lmer(ring.width_s ~ subsections* (age_s + PMDI_3yrlag_s + age_s:PMDI_3yrlag_s) +
#                                (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
#                                data=mdata, REML=FALSE)

# plot(fitted(cmn.rw.mod.simple.lmer), residuals(cmn.rw.mod.simple.lmer),
#      col= "red")
# abline(h=0, lty=2)
# lines(smooth.spline(fitted(cmn.rw.mod.simple.lmer), residuals(cmn.rw.mod.simple.lmer)))

################### 2. Resin Duct Density ###########################################





# cmn.rwden.mod.ri <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BA_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                            (1|tag), data=mdata, REML=FALSE)
# 
# cmn.rwden.mod.ri2 <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BA_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                            (1|spcode/tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.ri, cmn.rwden.mod.ri2)
# # AIC values indicate that the best model doesn't include species as random effect
# 
# cmn.rwden.mod.rsi <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BA_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                             (age_s|tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.ri, cmn.rwden.mod.rsi) # so random slope for age much better.
# 
# # try random slope for PMDI
# cmn.rwden.mod.rsi2 <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BA_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                              (age_s+PMDI_3yrlag_s|tag), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.rsi, cmn.rwden.mod.rsi2)
# 
# # Random slope is not better
# 
# # try calendar.year random intercept:
# cmn.rwden.mod.rsi3 <- lmer(duct.density.log_s ~ subsections*((age_s + PMDI_3yrlag_s) + BA_s + elev_s +ring.width_s + ldist_valley2_s) + mtn +
#                              (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year), data=mdata, REML=FALSE)
# 
# anova(cmn.rwden.mod.rsi, cmn.rwden.mod.rsi3)
# # AIC values are the same, so I will use the model that specifies the random
# # effects more in order to specify correct degrees of freedom.

cmn.rwden.mod.full <- mixed(duct.density.log ~ subsection*((age_s * (PMDI_3yrlag_s + ring_width_detrended_s)) +
                            BA_s + elev_s) + mtn +
                            (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                            data=mdata, REML=FALSE)

summary(cmn.rwden.mod.full)
anova(cmn.rwden.mod.full)


# So let's drop non significant interaction terms
cmn.rwden.mod.simple <- mixed(duct.density.log_s ~ subsection*ring_width_detrended_s*age_s +
                                age_s:ring_width_detrended_s + elev_s*subsection +
                                 (age_s+PMDI_3yrlag_s | tag) +
                                (1 | calendar.year), data=mdata, REML=FALSE)

summary(cmn.rwden.mod.simple)
anova(cmn.rwden.mod.simple)
print(xtable(anova(cmn.rwden.mod.simple)))


## # Create new dataframe with coefficients from summary
## coefs_ductdensity <- data.frame(coef(summary(cmn.rwden.mod.simple)))
## # use normal distribution to approximate p-value (not very conservative)
## coefs_ductdensity$p.z <- 2 * (1 - pnorm(abs(coefs_ductdensity$t.value)))

## # Add a new column with calculations with exact slope and intercept and not just relationship
## coefs_ductdensity$Estimate_calc <- length(coefs_ductdensity$Estimate) # will change numbers

## # select vector elements by position to do calculations
## coefs_ductdensity$Estimate_calc[1] <- coefs_ductdensity$Estimate[1]
## coefs_ductdensity$Estimate_calc[2] <- coefs_ductdensity$Estimate[1]+ coefs_ductdensity$Estimate[2]
## coefs_ductdensity$Estimate_calc[3] <- coefs_ductdensity$Estimate[1]+ coefs_ductdensity$Estimate[3]
## coefs_ductdensity$Estimate_calc[4] <- coefs_ductdensity$Estimate[4]
## coefs_ductdensity$Estimate_calc[5] <- coefs_ductdensity$Estimate[5]
## coefs_ductdensity$Estimate_calc[6] <- coefs_ductdensity$Estimate[6]
## coefs_ductdensity$Estimate_calc[7] <- coefs_ductdensity$Estimate[7]
## coefs_ductdensity$Estimate_calc[8] <- coefs_ductdensity$Estimate[4]+ coefs_ductdensity$Estimate[8]
## coefs_ductdensity$Estimate_calc[9] <- coefs_ductdensity$Estimate[4]+ coefs_ductdensity$Estimate[9]
## coefs_ductdensity$Estimate_calc[10] <- coefs_ductdensity$Estimate[5]+ coefs_ductdensity$Estimate[10]
## coefs_ductdensity$Estimate_calc[11] <- coefs_ductdensity$Estimate[5]+ coefs_ductdensity$Estimate[11]
## coefs_ductdensity$Estimate_calc[12] <- coefs_ductdensity$Estimate[6]+ coefs_ductdensity$Estimate[12]
## coefs_ductdensity$Estimate_calc[13] <- coefs_ductdensity$Estimate[6]+ coefs_ductdensity$Estimate[13]
## coefs_ductdensity$Estimate_calc[14] <- coefs_ductdensity$Estimate[7]+ coefs_ductdensity$Estimate[14]
## coefs_ductdensity$Estimate_calc[15] <- coefs_ductdensity$Estimate[7]+ coefs_ductdensity$Estimate[15]


# Post-hoc testing

# Subsections
lsmeans(cmn.rwden.mod.simple, pairwise~ subsection)
# Subsections and age
pairs(lstrends(cmn.rwden.mod.simple, "subsection", var="age_s"))
# Subsections and ring width
pairs(lstrends(cmn.rwden.mod.simple, "subsection", var="ring_width_detrended_s"))
# Subsections and elevation
pairs(lstrends(cmn.rwden.mod.simple, "subsection", var="elev_s"))


# # Create an lmer one to use to test heteroscedasticity

  # cmn.rwden.mod.simple.lmer <- lmer(duct.density.log_s ~ subsections*(age_s + elev_s + ring.width_s + PMDI_3yrlag_s + age_s:ring.width_s) +
  #                               subsections:age_s:PMDI_3yrlag_s + age_s:ring.width_s+
  #                               (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
  #                               data=mdata, REML=FALSE)
  # 
  # plot(fitted(cmn.rwden.mod.simple.lmer), residuals(cmn.rwden.mod.simple.lmer),
  #      col="red")
  # abline(h=0, lty=2)
  # lines(smooth.spline(fitted(cmn.rwden.mod.simple.lmer), residuals(cmn.rwden.mod.simple.lmer)))
