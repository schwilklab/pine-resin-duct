# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

library(ggplot2)
library(lme4)
source("read_all.R")
source("ggplot-theme.R")


# Create dataframe with information unique to each tree
#unique.trees <- mdata %>% group_by(tag) %>% top_n(1, age) 


## Ring width model ##

####### Figure 1: Drought and Ring Width #########

## rw_year_df <- mutate(mdata, rw_resid=residuals(lmer(ring_width_detrended ~ subsection*BAF_s +
##                            (PMDI_3yrlag_s | tag) + (1 | calendar.year), 
##                            data=mdata, REML=FALSE))
## )

## rw_year_df <- rw_year_df %>% group_by(subsection, calendar.year) %>%
##   summarize(rw_resid = mean(rw_resid), pmdi= mean(PMDI_3yrlag))

rw_year_df <- mdata %>% group_by(subsection, calendar.year) %>%
   summarize(rw_resid = mean(ring_width_detrended), pmdi= mean(PMDI_3yrlag))


# figure with each point a calendar year
ggplot(rw_year_df, aes(pmdi, rw_resid, shape=subsection, fill=subsection, linetype= subsection)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_point(size=2) +
  scale_fill_manual(values = c(NA, "gray90", "gray60"), guide_legend(title = "Subsection"))+
  scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
  scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, color="black", size=1.1) +
  ylab("Detrended ring width") +
  xlab("PMDI")+
  pubtheme.nogridlines


## All subsections strongly sensitive to PMDI


########################################
# resin duct density

####### Figure 2: Influence of Age on Resin Duct Density #########
ggplot(mdata, aes(age, duct.density)) + geom_point() +
  facet_grid(. ~ subsection)


ductdensity_df <- mdata %>% group_by(subsection) %>% mutate(age_c = age - mean(age))
getcoefs_dd <- function(df) {
  m <- lm(duct.density.log ~ age_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2], R2 = summary(m)$adj.r.squared))
}

duct_mods <- ductdensity_df %>% group_by(subsection, spcode, tag, mtn) %>% do(getcoefs_dd(.))
ggplot(duct_mods, aes(subsection, s)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() +
  xlab("Subsection") +
  ylab("Slope of regression \n of resin duct density with age") +
  annotate("text", x = 1, y = .07, label = "a") +
  annotate("text", x = 2, y = .07, label = "a") +
  annotate("text", x = 3, y = .07, label = "b") +
  pubtheme.nogridlines
# So it does look like Strobus does decrease at a greater rate than the other two subsection, 
# but it's not by a large amount.

ggplot(duct_mods, aes(subsection, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn) + xlab("Subsection") +
  ylab("Age coefficients with resin duct density as response variable") +
  pubtheme.nogridlines

####### Figure 3: Ring width and age on resin duct density #########

## Age

bytree <- mdata %>%  mutate(age_f = factor(age > 15, labels=c("< 15 yrs", "> 15 yrs"))) %>%
                   group_by(subsection, tag, age_f, elev) %>%
                   summarize(rw = mean(ring_width_detrended), dd = mean(duct.density))

ggplot(bytree, aes(subsection, dd)) + geom_boxplot() + facet_grid(. ~ age_f)

bytree <- mdata %>%  mutate(age_f = cut(age, breaks = c(0, 15, 30, 250))) %>%
                   group_by(subsection, tag, age_f, elev) %>%
                   summarize(rw = mean(ring_width_detrended), dd = mean(duct.density))

ggplot(bytree, aes(subsection, dd)) + geom_boxplot() + facet_grid(. ~ age_f)


#### Effect of ring width on resin duct density ###

newdata <- mdata %>% 
  mutate(age_f = factor(age > 15, labels=c("< 15 yrs", "> 15 yrs"))) 
ggplot(newdata, aes(ring_width_detrended, duct.density)) + geom_point() + facet_grid(subsection ~ age_f)



ductdensity_rw_df <- mdata %>% mutate(age_f = factor(age <= 10, labels=c("< 10 yrs", "> 10 yrs"))) %>%
  group_by(subsection, age_f)

getcoefs_dd_rw <- function(df) {
  m <- lm(duct.density.log ~ ring_width_detrended, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2], R2 = summary(m)$adj.r.squared))
}

duct_mods_rw <- ductdensity_rw_df %>% group_by(tag, age_f, subsection, spcode, mtn) %>%
  do(getcoefs_dd_rw(.)) # %>% filter(!s>= 24) %>% filter(!s <= -25)

# Filter by R2 >.15
ggplot(filter(duct_mods_rw , R2 >= .15), aes(age_f, s)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() + 
  scale_x_discrete() +
  facet_grid(. ~ subsection) +
  ylab("Slope of ring width and \n resin duct density correlation") +
  xlab("") +
  pubtheme.nogridlines


## or


ggplot(mdata, aes(age, duct.density)) + geom_point() +
  geom_smooth(method="lm") + 
  facet_grid(subsection ~ cut(ring_width_detrended, breaks=c(-1,0,1)))


ggplot(mdata, aes(elev, duct.density)) + geom_point() +
  geom_smooth(method="lm") + 
  facet_grid(subsection ~ cut(age, breaks=c(0,10,300)))



ggplot(mdata, aes(ring_width_detrended, duct.density)) + geom_point() +
  geom_smooth(method="lm") + 
  facet_grid(subsection ~ cut(age, breaks=c(0, 10, 300))
)

ggplot(mdata, aes(PMDI_3yrlag, duct.density.log)) + geom_point() +
  geom_smooth(method="lm") + 
  facet_grid(subsection ~ .)

ggplot(filter(mdata, subsection=="Strobus"), aes(ring_width_detrended, duct.density)) + geom_point() +
  geom_smooth(method="lm")




####### Figure 4 #########

# Resin duct density by subsection accounting for all other factors
density_mod <- lmer(duct.density.log ~ age_s + elev_s + ring_width_detrended_s +
                      age_s:ring_width_detrended_s +
                      (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                              data=mdata, REML=FALSE)

mdata$age_elev_rw_resid <- residuals(density_mod)
dd_subs_df <- mdata %>% group_by(tag, subsection) %>%
  summarize(ducts_resid= mean(age_elev_rw_resid))

ggplot(dd_subs_df, aes(subsection, ducts_resid)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() +
  annotate("text", x = 1, y = .11, label = "a") +
  annotate("text", x = 2, y = .11, label = "b") +
  annotate("text", x = 3, y = .11, label = "b") +
  xlab("Subsection") +
  ylab("Mean residual \n resin duct density") +
  pubtheme.nogridlines
# So Cembroides truely does have the larger resin duct density values


bytree <- mdata %>% group_by(tag, spcode, subsection) %>%
  summarize(dd = mean(duct.density.log))
ggplot(bytree, aes(subsection, dd)) + geom_boxplot() + ylab("Mean resin duct density")