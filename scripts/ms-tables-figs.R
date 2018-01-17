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

# Include the calendar year as random intercept
cmn.rw.mod.full <- mixed(ring_width_detrended ~ subsection*(PMDI_3yrlag_s + BA_s + elev_s) +
                           BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s +
                           (PMDI_3yrlag_s | tag) + (1 | calendar.year),
                         data=mdata, REML=FALSE)
saveRDS(cmn.rw.mod.full, "../results/rw_mod_full.RDS")
#cmn.rw.mod.full <- readRDS("../results/rw_mod_full_kr.RDS")
rw.coef.tab <- summary(cmn.rw.mod.full)$coefficients
rw.anova.tab <- anova(cmn.rw.mod.full)
rw.coef.tab
rw.anova.tab
print(xtable(rw.coef.tab), type="html", file="../results/rw_coef_tab.html")
print(xtable(rw.anova.tab), type="html", file="../results/rw_anova_tab.html")

system("pandoc -f html -t odt -o ../results/rw_coef_tab.odt ../results/rw_coef_tab.html")
system("pandoc -f html -t odt -o ../results/rw_anova_tab.odt ../results/rw_anova_tab.html")

################### 2. Resin Duct Density ###########################################
cmn.rdd.mod.full <- mixed(duct.density ~
                            subsection*((age_s * (PMDI_3yrlag_s + ring_width_detrended_s)) +
                            BA_s + elev_s)  +
                            BA_s:PMDI_3yrlag_s + BA_s:elev_s + PMDI_3yrlag_s:elev_s + 
                            (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                          data=mdata, REML=FALSE)
writeRDS(cmn.rdd.mod.full, "../results/rdd_mod_full_kr.RDS")
# cmn.rdd.mod.full <- loadRDS("../results/rdd_mod_full.RDS")
rdd.coef.tab <- summary(cmn.rdd.mod.full)$coefficients
rdd.anova.tab <- anova(cmn.rdd.mod.full)
rdd.coef.tab
rdd.anova.tab
print(xtable(rdd.coef.tab), type="html", file="../results/rdd_coef_tab.html")
print(xtable(rdd.anova.tab), type="html", file="../results/rdd_anova_tab.html")

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


# Fig 2: Ring width by PMDI
rw_year_df <- mdata %>% group_by(subsection, calendar.year) %>%
   summarize(rw_resid = mean(ring_width_detrended), pmdi= mean(PMDI_3yrlag))

# figure with each point a calendar year
fig2 <- ggplot(rw_year_df, aes(pmdi, rw_resid, color= subsection)) +
  #  geom_abline(intercept = 0, slope = 0, color= "gray") +
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
        legend.justification = c("right", "bottom"),
        legend.key.height = unit(0.6, "lines"),
        legend.position = c(0.99, 0.01),
        legend.title=element_blank())
ggsave("../results/fig2_ring_width_PMDI.pdf", plot=fig2, width=col1, height=0.9*col1, units="cm")
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
  geom_point(size=ptsize, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors)+
  facet_grid(. ~ age_f) +
  geom_smooth(method="lm", size = lnsize, se=FALSE) +
  ylim(c(0,300)) +
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
