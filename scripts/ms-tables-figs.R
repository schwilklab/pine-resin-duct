# ms-tables-figs.R

# mixed models, model results tables and ms figures. This script dfoes not
# include explorator analysis in which three-way interactions were investigated
# and discarded. Not models with "mtn" as a fixed effect as it was never
# important (captured by species in gerneal, most likely).

library(ggplot2)
library(lme4)
#library(MuMIn)
library(afex) # for p values in lmer/glmer models. 
#afex_options(method_mixed="LRT") # fast and dirty
#afex_options(method_mixed="PB") # Bootstrapping.
afex_options(method_mixed="KR") #  Kenward-Roger approximation for degrees-of-freedom
## KR is best for controlling type I error. In our data, these methods seem equivalent
## with one exception: the PMDI:BA interaction in the rw model. But this is not
## an important part of our story in any case and the interaction with
## subsection is always significant.

library(xtable)

source("ggplot-theme.R")
source("read_all.R")

################### 1. Ring Width ###########################################

# Include the calendar year as random intercept and age slope
cmn.rw.mod.full <- mixed(ring_width_detrended ~  subsection*(PMDI_3yrlag_s + BA_s + elev_s) +
                          BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                          (PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
                          data=mdata, REML=FALSE)
saveRDS(cmn.rw.mod.full, "../results/rw_mod_full.RDS")
#cmn.rw.mod.full <- readRDS("../results/rw_mod_full_kr.RDS")
rw.coef.tab <- summary(cmn.rw.mod.full)$coefficients
rw.anova.tab <- anova(cmn.rw.mod.full)
rw.coef.tab
rw.anova.tab
print(xtable(rw.coef.tab, digits=3), type="html", file="../results/rw_coef_tab.html")
print(xtable(nice(cmn.rw.mod.full)), type="html", file="../results/rw_anova_tab.html")

system("pandoc -f html -t odt -o ../results/rw_coef_tab.odt ../results/rw_coef_tab.html")
system("pandoc -f html -t odt -o ../results/rw_anova_tab.odt ../results/rw_anova_tab.html")

################### 2. Resin Duct Density ###########################################
cmn.rdd.mod.full <-  mixed(duct.density.log ~
                     subsection*(age_s + ring_width_detrended_s + BA_s + elev_s + PMDI_3yrlag_s) +
                     age_s:(PMDI_3yrlag_s +  ring_width_detrended_s + elev_s) +
                     BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                     subsection:age_s:PMDI_3yrlag_s +
                     #subsection:age_s:ring_width_detrended_s +
                            (PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
                     data=mdata, REML=FALSE)

#writeRDS(cmn.rdd.mod.full, "../results/rdd_mod_full_kr.RDS")

# run actual mixed model fitting on cluster and read from RDS file:
cmn.rdd.mod.full <- readRDS("../results/rdd_mod_full_kr.RDS")
rdd.coef.tab <- summary(cmn.rdd.mod.full)$coefficients
rdd.anova.tab <- anova(cmn.rdd.mod.full)
rdd.coef.tab
rdd.anova.tab
print(xtable(rdd.coef.tab), type="html", file="../results/rdd_coef_tab.html")
print(xtable(nice(rdd.anova.tab)), type="html", file="../results/rdd_anova_tab.html")

system("pandoc -f html -t odt -o ../results/rdd_coef_tab.odt ../results/rdd_coef_tab.html")
system("pandoc -f html -t odt -o ../results/rdd_anova_tab.odt ../results/rdd_anova_tab.html")


########################################################################
## Figures
########################################################################
ptsize <- 1.5
lnsize <- 0.8


# Fig 1: Map of mtn ranges
library(maps)
library(ggmap)
#states <- map_data("state")
#txnm <-  subset(states, region %in% c("texas", "new mexico"))
## ggplot(data = txnm) + 
##   geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") +
##   xlab("Longitude") + ylab("Latitude") +
##   coord_fixed(xlim=c(-107,-102), ylim=c(29,32),  ratio=1.3) +
##   geom_point(aes(x=lon, y=lat), data=trees, size=0.1)

mtns <- trees %>% group_by(mtn) %>% summarize(lat=mean(lat)-0.15, lon = mean(lon)) %>%
  mutate(mtn = recode(mtn, CM="Chisos Mtns", DM="Davis Mtns", GM="Guadalupe Mtns"))
mtns$lat[1] <- mtns$lat[1] + 0.3

#txmap <- get_map(geocode("Texas"), zoom=6, color="bw")
#saveRDS(txmap, "../results/txmap.RDS")
txmap <- readRDS("../results/txmap.RDS")
m <- ggmap(txmap)
fig1 <- m +  xlab("Longitude") + ylab("Latitude") +
  #  coord_fixed(xlim=c(-107,-102), ylim=c(29,32),  ratio=1.3) +
  scale_x_continuous(limits= c(-106.8,-102.8)) +
  scale_y_continuous(limits= c(29.1,32)) +
  geom_point(aes(x=lon, y=lat), alpha=0.5, data=trees, size=0.1) +
  geom_text(aes(x=lon, y=lat, label=mtn), data=mtns) +
  pubtheme.nogridlines 
ggsave("../results/fig1_map.pdf", plot=fig1, width=col1, height=0.9*col1, units="cm")

## CMmap <-  get_map(location = c(lon=mean(subset(trees, mtn=="CM")$lon),
##                                lat=mean(subset(trees, mtn=="CM")$lat)), zoom=10, color="bw")
## ggmap(CMmap) +
##     geom_point(aes(x=lon, y=lat), alpha=0.5, data=trees, size=1)


## # Fig 2: Ring width by PMDI
## rw_year_df <- mdata %>% group_by(subsection, calendar.year) %>%
##    summarize(rw_resid = mean(ring_width_detrended), pmdi= mean(PMDI_3yrlag))

## # figure with each point a calendar year
## fig2 <- ggplot(rw_year_df, aes(pmdi, rw_resid, color= subsection)) +
##   #  geom_abline(intercept = 0, slope = 0, color= "gray") +
##   geom_point(size=ptsize, alpha=0.8, shape=16) +
##   scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
##   ## scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
##   ## scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
##   geom_smooth(method="lm", se=FALSE, size=lnsize) +
##   ylab("Detrended ring width (cm)") +
##   xlab("Modified Palmer drought severity index")+
##   #    ylim(c(-0.1, 0.1)) +
##   pubtheme.nogridlines +
##   theme(legend.text = element_text(face="italic"),
##         legend.justification = c("right", "bottom"),
##         legend.key.height = unit(0.6, "lines"),
##         legend.position = c(0.99, 0.01),
##         legend.title=element_blank())
## ggsave("../results/fig2_ring_width_PMDI.pdf", plot=fig2, width=col1, height=0.9*col1, units="cm")
## ## All subsections strongly sensitive to PMDI



# Fig 2: Ring width by PMDI by elev
rw_year_df <- mdata %>% mutate(elev_f = cut(elev, breaks=2, labels=c("< 2000 m", "> 2000 m"))) %>%
  group_by(subsection, calendar.year, elev_f) %>%
   summarize(rw_resid = mean(ring_width_detrended), pmdi= mean(PMDI_3yrlag))

# figure with each point a calendar year
fig2 <- ggplot(rw_year_df, aes(pmdi, rw_resid, color= subsection)) +
  #  geom_abline(intercept = 0, slope = 0, color= "gray") +
  facet_grid(. ~ elev_f) +
  geom_point(size=ptsize, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  ## scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
  ## scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, size=lnsize) +
  ylab("Detrended ring width (cm)") +
  xlab("Modified Palmer drought severity index")+
  #    ylim(c(-0.1, 0.1)) +
  pubtheme.nogridlines +
  theme(legend.text = element_text(face="italic"),
        legend.justification = c("left", "bottom"),
        legend.key.height = unit(0.6, "lines"),
        legend.position = c(0.01, 0.01),
        legend.title=element_blank())
ggsave("../results/fig2_ring_width_PMDI_elev.pdf", plot=fig2, width=col1, height=0.9*col1, units="cm")
## All subsections strongly sensitive to PMDI


## devtools::install_github("wilkelab/colorblindr")
#install.packages("colorspace", repos = "http://R-Forge.R-project.org")
## devtools::install_github("clauswilke/colorblindr")
#library(colorblindr)
#cvd_grid(fig1)
# Color theme is ok


# Figure 3: Ring width and BA by subsection
rw_ss_df <- mdata %>% group_by(tag, spcode, subsection) %>%
   summarize(rw = mean(ring_width_detrended), BA= mean(BA))

# figure with each point a calendar year
fig3 <- ggplot(rw_ss_df, aes(BA, rw, color= subsection)) +
  #  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_jitter(size=ptsize, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  ## scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
  ## scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, size=lnsize) +
  ylab("Detrended ring width (cm)") +
  xlab(bquote("Stand basal area ("*m^2~ha^-1*")")) +
  pubtheme.nogridlines +
  theme(legend.text = element_text(face="italic"),
        legend.justification = c("right", "top"),
        legend.key.height = unit(0.6, "lines"),
        legend.position = c(0.99, 0.99),
        legend.title=element_blank())
ggsave("../results/fig3_ring_width_BA.pdf", plot=fig3, width=col1, height=.9*col1, units="cm")


## Figure 4: Resin duct density by subsection
bytree <- mdata %>% group_by(tag, spcode, subsection) %>%
  summarize(dd = mean(duct.density))

fig4 <- ggplot(bytree, aes(subsection, dd, fill=subsection)) +
  scale_y_log10(breaks=pretty_breaks()) +
  geom_boxplot(outlier.color=NA) +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  xlab("") + #"Subsection") + +
  scale_fill_manual(values = schwilkcolors, guide=FALSE) +
  pubtheme.nogridlines +
  theme(axis.text.x = element_text(face="italic"))
ggsave("../results/fig4_rdd_subsection.pdf", plot=fig4, width=col1, height=0.9*col1, units="cm")
                                                            

## Fig 5: Resin duct density as a function of detrended ring width, grouped by
## cambial age
bytree <- mdata %>%  mutate(age_f = factor(age > 15, labels=c("< 15 yrs", "> 15 yrs"))) %>% 
                   group_by(subsection, tag, age_f, elev) %>%
                   summarize(rw = mean(ring_width_detrended), dd = mean(duct.density))

# rdd by elevation and subsection and age
fig5 <- ggplot(bytree, aes(rw, dd, color=subsection)) +
  scale_y_log10(breaks=pretty_breaks(), limits = c(NA,400)) +
  geom_point(size=ptsize, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors)+
  facet_grid(. ~ age_f) +
  geom_smooth(method="lm", size = lnsize, se=FALSE) +
  xlim(c(-0.1, 0.2)) +
  xlab("Detrended ring width (cm)") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  pubtheme.nogridlines +
  theme(legend.text = element_text(face="italic"),
        legend.justification = c(1, 1),
        legend.key.height = unit(0.6, "lines"),
        legend.position = c(0.99, 0.99),
        legend.title=element_blank())

ggsave("../results/fig5_rdd_rw_age_subsection.pdf", plot=fig5, width=col1, height=0.9*col1, units="cm")

#############################################################################3
## Supplemental figs

# lmer version of rw model

cmn.rw.mod <- lmer(ring_width_detrended ~  subsection*(PMDI_3yrlag_s + BA_s + elev_s) +
                          BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                          (PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
                          data=mdata, REML=FALSE)

  ## duct.density.log ~
  ##                           subsection*((age_s * (ring_width_detrended_s)) +
  ##                                         BA_s + elev_s + PMDI_3yrlag_s)  +
  ##                           elev_s:age_s +
  ##                           BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s + 
  ##                           (age_s+PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
  ##                           data=mdata, REML=FALSE)


# make lmer version of rdd model
cmn.rdd.mod <- lmer(duct.density.log ~
                     subsection*(age_s + ring_width_detrended_s + BA_s + elev_s + PMDI_3yrlag_s) +
                     age_s:(PMDI_3yrlag_s +  ring_width_detrended_s + elev_s) +
                     BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                     subsection:age_s:PMDI_3yrlag_s +
                     #subsection:age_s:ring_width_detrended_s +
                            (PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
                          data=mdata, REML=FALSE)

## duct.density.log ~
##                             subsection*((age_s * (PMDI_3yrlag_s + ring_width_detrended_s)) +
##                             BA_s + elev_s)  +
##                             BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s + 
##                             (age_s+PMDI_3yrlag_s | tag) + (1 + age_s | calendar.year),
##                           data=mdata, REML=FALSE)


newdata <-  mdata %>% mutate(prw = predict(cmn.rw.mod),
                             prdd = exp(predict(cmn.rdd.mod)),
                             age_f = factor(cut(age, breaks=2)),
                             pmdi_f = factor(cut(PMDI_3yrlag, breaks=2)),
                             elev_f = factor(cut(elev, breaks=2)),
                             )



#Fig S3
# intearction of pmdi and elev on rw ?

figS3 <- ggplot(newdata, aes(PMDI_3yrlag, prw, color = elev_f)) +
  geom_smooth(method="lm") +
 # geom_point() +
  labs(y = expression("Predicted detrened ring width (cm)"),
       x = "PMDI")+
  scale_color_manual(values = schwilkcolors, guide_legend(title = "elevation"))+
 theme(legend.justification = c("right", "top"),
        legend.key.height = unit(0.6, "lines"),
        legend.position = c(0.99, 0.99),
       legend.title=element_blank()) + 
  pubtheme.nogridlines


ggsave("../results/figS3_rw_elev_pmdi_predicted.png", plot=figS3, width=col1,
       height=0.9*col1, units="cm")

figS3




# Fig S4 rdd by PMDI, subsection and age
# effects plot to look at PMDI:age
# interaction plot
figS4 <- ggplot(newdata, aes(age, prdd, color = pmdi_f)) +
  geom_smooth(method="lm") +
 # geom_point() +
  labs(y = expression("Predicted resin duct density ("*cm^-2*")"),
       x = "age",
       color = "PMDI") +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "PMDI"))+
 theme(legend.justification = c("right", "top"),
        legend.key.height = unit(0.6, "lines"),
        legend.position = c(0.99, 0.99),
       legend.title=element_blank()) + 
  scale_y_log10(breaks=pretty_breaks()) +
  pubtheme.nogridlines +
  scale_y_log10(breaks=pretty_breaks())

ggsave("../results/figS4_rdd_age_pmdi_predicted.png", plot=figS4, width=col1,
       height=0.9*col1, units="cm")

figS4

############################################################33
#Fig S5  # check if needed (Age:elev?)

bytree <- data %>% group_by(subsection, tag, age_f, elev) %>%
  summarize(rw = mean(ring_width_detrended), dd = mean(duct.density))

