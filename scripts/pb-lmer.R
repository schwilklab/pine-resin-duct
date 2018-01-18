#!/usr/bin/env Rscript

# temp script to run parametric boostrap versions of mixed()

library(lme4)
#library(MuMIn)
library(afex) # for p values in lmer/glmer models. 
#afex_options(method_mixed="LRT") # fast and dirty
afex_options(method_mixed="PB") # Bootstrapping.
#afex_options(method_mixed="KR") #  Kenward-Roger approximation for degrees-of-freedom
## KR is best for controlling type I error. In our data, these methods seem equivalent
## with one exception: the PMDI:BA interaction in the rw model. But this is not
## an important part of our story in any case and the interaction with
## subsection is always significant.

library(xtable)

source("ggplot-theme.R")
source("read_all.R")

library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
tcl <- makeCluster(no_cores)

################### 1. Ring Width ###########################################

# Include the calendar year as random intercept
## cmn.rw.mod.full <- mixed(ring_width_detrended ~ subsection*(PMDI_3yrlag_s + BA_s + elev_s) +
##                            BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
##                            (PMDI_3yrlag_s | tag) + (1 | calendar.year),
##                          data=mdata, REML=FALSE)
## saveRDS(cmn.rw.mod.full, "../results/rw_mod_full.RDS")
## #cmn.rw.mod.full <- readRDS("../results/rw_mod_full_kr.RDS")
## rw.coef.tab <- summary(cmn.rw.mod.full)$coefficients
## rw.anova.tab <- anova(cmn.rw.mod.full)
## rw.coef.tab
## rw.anova.tab
## print(xtable(rw.coef.tab), type="html", file="../results/rw_coef_tab.html")
## print(xtable(nice(cmn.rw.mod.full)), type="html", file="../results/rw_anova_tab.html")

## system("pandoc -f html -t odt -o ../results/rw_coef_tab.odt ../results/rw_coef_tab.html")
## system("pandoc -f html -t odt -o ../results/rw_anova_tab.odt ../results/rw_anova_tab.html")

################### 2. Resin Duct Density ###########################################
cmn.rdd.mod.full <- mixed(duct.density ~
                            subsection*((age_s * (PMDI_3yrlag_s + ring_width_detrended_s)) +
                            BA_s + elev_s)  +
                            BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s + 
                            (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                          data=mdata, REML=FALSE, cl=tcl)
saveRDS(cmn.rdd.mod.full, "../results/rdd_mod_full_pb.RDS")
# cmn.rdd.mod.full <- loadRDS("../results/rdd_mod_full.RDS")
rdd.coef.tab <- summary(cmn.rdd.mod.full)$coefficients
rdd.anova.tab <- anova(cmn.rdd.mod.full)
rdd.coef.tab
rdd.anova.tab
print(xtable(rdd.coef.tab), type="html", file="../results/rdd_coef_tab.html")
print(xtable(nice(cmn.rdd.mod.full)), type="html", file="../results/rdd_anova_tab.html")

system("pandoc -f html -t odt -o ../results/rdd_coef_tab.odt ../results/rdd_coef_tab.html")
system("pandoc -f html -t odt -o ../results/rdd_anova_tab.odt ../results/rdd_anova_tab.html")
