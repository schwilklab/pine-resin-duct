# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

library(ggplot2)
library(lme4)
source("read_all.R")
source("ggplot-theme.R")


####### Figure 1: Drought and Ring Width #########

rw_year_df <- mdata %>% group_by(subsection, calendar.year) %>%
   summarize(rw_resid = mean(ring_width_detrended), pmdi= mean(PMDI_3yrlag))


# figure with each point a calendar year
ggplot(rw_year_df, aes(pmdi, rw_resid, color= subsection)) +
#  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_point(size=1.5, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  ## scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
  ## scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, size=0.8) +
  ylab("Detrended ring width (cm)") +
    xlab("Modified Palmer drought severity index")+
#    ylim(c(-0.1, 0.1)) +
  prestheme.nogridlines +
   theme(legend.justification = c("right", "bottom"),
        legend.key.height = unit(0.5, "lines"),
        legend.position = c(1, 0),
        legend.title=element_blank())
ggsave("../results/ring_width_PMDI.pdf", width=8, height=7.25, units="cm")
## All subsections strongly sensitive to PMDI


# ring width and BA by subsection
rw_ss_df <- mdata %>% group_by(tag, spcode, subsection) %>%
   summarize(rw = mean(ring_width_detrended), BA= mean(BA))


# figure with each point a calendar year
ggplot(rw_ss_df, aes(BA, rw, color= subsection)) +
#  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_jitter(size=1.5, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  ## scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
  ## scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, size=0.9) +
  ylab("Detrended ring width (cm)") +
  xlab(bquote("Basal area ("*m^2~ha^-1*")")) +
  prestheme.nogridlines +
  theme(legend.justification = c("right", "top"),
        legend.key.height = unit(0.5, "lines"),
        legend.position = c(1, 1),
        legend.title=element_blank())
ggsave("../results/ring_width_BA.pdf", width=8, height=7.25, units="cm")


########################################
# resin duct density

## by subsection

bytree <- mdata %>% group_by(tag, spcode, subsection) %>%
  summarize(dd = mean(duct.density))


ggplot(bytree, aes(subsection, dd, fill=subsection)) +
  geom_boxplot(outlier.color=NA) +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  xlab("") + #"Subsection") + +
  scale_fill_manual(values = schwilkcolors, guide=FALSE) +
  prestheme.nogridlines
ggsave("../results/rdd_subsection.pdf", width=8, height=7.25, units="cm")
                                                            


####### By tree, grouped by cambial age #########
bytree <- mdata %>%  mutate(age_f = factor(age > 15, labels=c("< 15 yrs", "> 15 yrs"))) %>% 
                   group_by(subsection, tag, age_f, elev) %>%
                   summarize(rw = mean(ring_width_detrended), dd = mean(duct.density))

ggplot(bytree, aes(subsection, dd, fill=subsection)) +
  geom_boxplot(outlier.color=NA) +
  facet_grid(. ~ age_f) +
  scale_fill_manual(values = schwilkcolors, guide=FALSE) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ylim(c(0,300)) +
  xlab("") + #"Subsection") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  prestheme.nogridlines
ggsave("../results/rdd_age_subsection.pdf", width=10, height=7.5, units="cm")


# rdd by elevation and subsection and age
ggplot(bytree, aes(elev, dd, color=subsection)) +
   geom_point(size=1.5, alpha=0.8, shape=16) +
  facet_grid(. ~ age_f) +
   theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, size=0.9) +
  ylim(c(0,300)) +
  xlab("Elevation (m)") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  prestheme.nogridlines +
  theme(legend.justification = c(1, 1),
        legend.key.height = unit(0.5, "lines"),
        legend.position = c(1, 1),
        legend.title=element_blank())
ggsave("../results/rdd_elev_age_subsection.pdf", width=10, height=7.5, units="cm")


# rdd by subsection, age and rw
ggplot(bytree, aes(rw, dd, color=subsection)) +
  geom_point(size=1.5, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors)+
  facet_grid(. ~ age_f) +
  geom_smooth(method="lm", size = 0.9, se=FALSE) +
  ylim(c(0,300)) +
  xlim(c(-0.1, 0.2)) +
  xlab("Detrended ring width (cm)") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  prestheme.nogridlines +
  theme(legend.justification = c(1, 1),
        legend.key.height = unit(0.5, "lines"),
        legend.position = c(1, 1),
        legend.title=element_blank())

ggsave("../results/rdd_rw_age_subsection.pdf", width=10, height=7.5, units="cm")


  ## guides(fill=guide_legend(keywidth=0.1,
  ##                          keyheight=0.1,
  ##                          default.unit="cm"))


###########################################################################
####### By tree, grouped by detrended ring width #########

bytree <- mdata %>%
  mutate(rw_f = factor(ring_width_detrended > 0, labels=c("Narrow", "Wide"))) %>%
                   group_by(subsection, tag, spcode, rw_f, elev) %>%
                   summarize(dd = mean(duct.density))

ggplot(bytree, aes(elev, dd, color=subsection)) +
  geom_point(size=1.5, alpha=0.8, shape=16) +
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
#  facet_grid(. ~ rw_f) +
  geom_smooth(method="lm", se=FALSE, size=0.9) + #, data=filter(bytree, subsection!="Cembroides")) +
  ylim(c(0,300)) +
  xlab("Elevation (m)") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  prestheme.nogridlines +
   theme(legend.justification = c("left", "top"),
        legend.position = c(0, 1),
        legend.title=element_blank())
ggsave("../results/rdd_elev_rwf_subsection.pdf", width=8, height=7.25, units="cm")



###########################################################################
####### By tree, grouped by elevation #########

### Not as useful

bytree <- mdata %>%  mutate(elev_f = factor(elev > 2000, labels=c("< 2000 m", "> 2000 m"))) %>%
                   group_by(subsection, tag, spcode, elev_f) %>%
                   summarize(dd = mean(duct.density), rw= mean(ring_width_detrended))

ggplot(bytree, aes(rw, dd, color=subsection)) + geom_point(size=1, alpha=0.8)+
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  facet_grid(. ~ elev_f) +
  geom_smooth(method="lm", se=FALSE, size=1) +
  ylim(c(0,300)) +
  xlab("Detrended ring width (cm)") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  prestheme.nogridlines +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.title=element_blank())
ggsave("../results/rdd_rw_elevf_subsection.pdf", width=9, height=7, units="cm")



###########################################################################
####### By cambial age grouped by tree id #########

byage <- mdata %>%   mutate(rw_f = factor(ring_width_detrended > 0, labels=c("Narrow", "Wide"))) %>%
  group_by(subsection, tag, spcode, age, elev, rw_f) %>%
                   summarize(dd = mean(duct.density))

ggplot(byage, aes(age, dd, color=subsection)) + geom_point(size=2)+
  scale_color_manual(values = schwilkcolors, guide_legend(title = "Subsection"))+
  facet_grid(. ~ rw_f) +
  geom_smooth(aes(group=tag), method="lm", se=FALSE, size=1) +
  ylim(c(0,300)) +
  xlab("Cambial age (years)") +
  ylab(expression("Resin duct density ("*cm^-2*")")) +
  prestheme.nogridlines
ggsave("../results/rdd_byage_subsection.pdf", width=10, height=7, units="cm")



### age and rw residuals appraoch

# Not useful, models fit terribly
## getcoefs_dd <- function(df) {
##   m <- lm(duct.density ~ ring_width_detrended, data=df)
##   return(data.frame(i = coef(m)[1], s = coef(m)[2], R2 = summary(m)$adj.r.squared))
## }
## duct_mods <- mdata %>%
##   mutate(age_f = factor(age > 15, labels=c("< 15 yrs", "> 15 yrs"))) %>%
##   group_by(subsection, spcode, tag, mtn, elev, age_f) %>% do(getcoefs_dd(.)) %>%
##   filter(R2 > 0.2)
## ggplot(duct_mods, aes(subsection, s)) +
##   facet_grid(. ~ age_f) +
##   geom_abline(intercept = 0, slope = 0, color= "gray") +
##   #geom_boxplot() +
##   geom_jitter() +
##   ylim(c(-300, 1000)) +
##   xlab("Subsection") +
##   ylab("Slope of regression \n of resin duct density with ring width") +
##   pubtheme.nogridlines
