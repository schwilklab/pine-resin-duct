#!/usr/bin/env Rscript

# Run lmer models on a computing cluster. Necessary manily because of high
# memory demands of K-R approximation.

library(lme4)
library(afex) # for p values in lmer/glmer models.
afex_options(method_mixed="KR") #  Kenward-Roger approximation for degrees-of-freedom
## KR is best for controlling type I error. In our data, these methods seem equivalent
## with one exception: the PMDI:BA interaction in the rw model. But this is not
## an important part of our story in any case and the interaction with
## subsection is always significant.

source("read_all.R")

library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
tcl <- makeCluster(no_cores)

################### 1. Ring Width ###########################################

# Include the calendar year as random intercept
cmn.rw.mod.full <- mixed(ring_width_detrended ~ subsection*(PMDI_3yrlag_s + BA_s + elev_s) +
                           BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                           (PMDI_3yrlag_s | tag) + (1 | calendar.year),
                         data=mdata, REML=FALSE)
#saveRDS(cmn.rw.mod.full, "../results/rw_mod_full.RDS")
saveRDS(cmn.rw.mod.full, "/lustre/scratch/dschwilk/rw_mod_full_kr.RDS")


rw.coef.tab <- summary(cmn.rw.mod.full)$coefficients
rw.anova.tab <- anova(cmn.rw.mod.full)
rw.coef.tab
rw.anova.tab



################### 2. Resin Duct Density ###########################################
cmn.rdd.mod.full <-  mixed(duct.density.log ~
                     subsection*(age_s + ring_width_detrended_s + BA_s + elev_s + PMDI_3yrlag_s) +
                     age_s:(PMDI_3yrlag_s +  ring_width_detrended_s + elev_s) +
                     BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                     subsection:age_s:PMDI_3yrlag_s +
                     subsection:age_s:ring_width_detrended_s +
                     subsection:age_s:BA_s +
                     subsection:age_s:elev_s +
                     subsection:PMDI_3yrlag_s:elev_s +
                     subsection:ring_width_detrended_s+BA_s +
                     subsection:ring_width_detrended_s+elev_s +
                     subsection:ring_width_detrended_s+PMDI_3yrlag_s +
                     subsection:
                            (PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
                     data=mdata, REML=FALSE)

saveRDS(cmn.rdd.mod.full, "/lustre/scratch/dschwilk/rdd_mod_full_kr.RDS")
# cmn.rdd.mod.full <- loadRDS("../results/rdd_mod_full.RDS")
rdd.coef.tab <- summary(cmn.rdd.mod.full)$coefficients
rdd.anova.tab <- anova(cmn.rdd.mod.full)
rdd.coef.tab
rdd.anova.tab



