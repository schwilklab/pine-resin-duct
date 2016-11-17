# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

library(ggplot2)
library(lme4)
source("read_rings.R")
source("ggplot-theme.R")


# Create dataframe with information unique to each tree
unique.trees <- mdata %>% group_by(tag) %>% top_n(1, age) 


## Ring width model ##

####### Figure 1 #########

## Ring width by subsection, accounting for age and drought
age_pmdi_mod <- lmer(ring.width ~ age + PMDI_3yrlag + age:PMDI_3yrlag +
                       (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                        data=mdata, REML=FALSE)
mdata$rw_age_pmdi_resid <- residuals(age_pmdi_mod)
rw_subs_df <- mdata %>% group_by(tag, subsections) %>%
  summarize(rw_resid= mean(rw_age_pmdi_resid))


ggplot(rw_subs_df, aes(subsections, rw_resid)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() +
  xlab("Subsection") + ylab("Mean ring width residuals by tree") +
  pubtheme.nogridlines
# So not terribly dramatic. This has a relatively higher p-value, so it may be that it 
# actually isn't signficant.  


####### Figure 2 #########

### Effect of age by subsection on ring width ###
# center ages by subsection so we can make use of intercept as well as slope
age_df <- mdata %>% group_by(subsections) %>% mutate(age_c = age - mean(age))
getcoefs <- function(df) {
  m <- lm(ring.width ~ age_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2], R2 = summary(m)$adj.r.squared))
}
age_mods <- age_df %>% group_by(subsections, spcode, tag, mtn) %>% do(getcoefs(.))
ttt<- remove_outliers(age_mods$s)
ggplot(age_mods, aes(subsections, s)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  xlab("Subsection") +
  ylab("Age coefficients with ring width \n  as response variable") +
  geom_boxplot() +
  pubtheme.nogridlines
## rw tends to decrease with age. Slight tendency for this to happen more
## rapidly in ponderosae but driven by a few individual trees. Lets look:
ggplot(age_mods, aes(subsections, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn)
# Yes, driven entirely by some of the PIPO, not by PIAR5. PIPO in question
# are located in Guadalupe mountains.  


####### Figure 3 #########

#### Ring width by drought across subsections ####
age_mod <- lmer(ring.width ~ age_s + (1 + age_s | tag), data=mdata, REML=FALSE)
mdata$rw_age_resid <- residuals(age_mod)
rw_year_df <- mdata %>% group_by(subsections, calendar.year) %>%
  summarize(rw_resid = mean(rw_age_resid), pmdi= mean(PMDI_3yrlag))


# figure with each point a calendar year
ggplot(rw_year_df, aes(pmdi, rw_resid, shape=subsections, fill=subsections, linetype= subsections)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_point(size=2) +
  scale_fill_manual(values = c(NA, "gray90", "gray60"), guide_legend(title = "Subsection"))+
  scale_shape_manual(values= c(21,22,24), guide_legend(title = "Subsection"))+
  scale_linetype_manual(values= c(6,3,1), guide_legend(title = "Subsection"))+
  geom_smooth(method="lm", se=FALSE, color="black", size=1.1) +
  ylab("Residual ring width \n accounting for tree age") +
  xlab("Average PMDI Value over 3 Years ")+
  pubtheme.nogridlines
# Strobus most sensitive to pmdi These residuals are after ccounting for age
# effects with separate slopes and intercepts per tree. Altenratiely, one could
# run an overal age model, eg lm(ring.width ~ age_s, data=mdata)

########################################
# resin duct density

####### Figure 4 #########

# Resin duct density by subsection accounting for all other factors
density_mod <- lmer(duct.density.log_s ~ age_s + elev_s + ring.width_s + age_s:ring.width_s +
                                (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                              data=mdata, REML=FALSE)
mdata$age_elev_rw_resid <- residuals(density_mod)
dd_subs_df <- mdata %>% group_by(tag, subsections) %>%
  summarize(ducts_resid= mean(age_elev_rw_resid))

ggplot(dd_subs_df, aes(subsections, ducts_resid)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() +
  xlab("Subsection") +
  ylab("Mean residual logged \n resin duct density per tree") +
  pubtheme.nogridlines
# So Cembroides truely does have the larger resin duct density values

####### Figure 5 #########

# Effect of age on resin duct density
ductdensity_df <- mdata %>% group_by(subsections) %>% mutate(age_c = age - mean(age))
getcoefs_dd <- function(df) {
  m <- lm(duct.density.log ~ age_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2], R2 = summary(m)$adj.r.squared))
}

duct_mods <- ductdensity_df %>% group_by(subsections, spcode, tag, mtn) %>% do(getcoefs_dd(.))
ggplot(duct_mods, aes(subsections, s)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() +
  xlab("Subsection") +
  ylab("Age coefficients with resin duct \n density as response variable") +
  pubtheme.nogridlines
# So it does look like Strobus does decrease at a greater rate than the other two subsections, 
# but it's not by a large amount.

ggplot(duct_mods, aes(subsections, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn) + xlab("Subsection") +
  ylab("Age coefficients with resin duct density as response variable") +
  pubtheme.nogridlines
 # Looks like there are two big outliers for P. ponderosa in the GM


####### Figure 6 #########

#### Effect of ring width on resin duct density by age ###

# function to assign young and old values to rings
factor_age<- function(age) {
  if (age <= 10){
    return ("Young")
  }
  else
    return ("Old")
}

mdata$age_f <- NA
for(i in 1:length(mdata$age)) {
  mdata$age_f[i] <- factor_age(mdata$age[i])
}

mdata$age_f <- as.factor(mdata$age_f)

#### Effect of ring width on resin duct density ###
ductdensity_rw_df <- mdata %>% group_by(subsections, age_f) %>% mutate(ring.width_c = ring.width - mean(ring.width))
getcoefs_dd_rw <- function(df) {
  m <- lm(duct.density.log ~ ring.width_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2], R2 = summary(m)$adj.r.squared))
}

duct_mods_rw <- ductdensity_rw_df %>% group_by(subsections, age_f, spcode, tag, mtn) %>%
  do(getcoefs_dd_rw(.)) # %>% filter(!s>= 24) %>% filter(!s <= -25)

# Filter by R2 >.15
ggplot(filter(duct_mods_rw , R2 >= .15), aes(age_f, s)) +
  geom_abline(intercept = 0, slope = 0, color= "gray") +
  geom_boxplot() + 
  scale_x_discrete(limits=c("Young", "Old")) +
  facet_grid(.~subsections) +
  ylab("Resin duct density coefficients with \n ring width as response variable") +
  xlab("") +
  pubtheme.nogridlines






#### Erik's :

source("ggplot-theme.R")

## Ring width model #

# Ring width by subsections 

ggplot(mdata, aes(subsections, ring.width)) +
  geom_boxplot()

# Ring width by age seperated by subsections
ggplot(mdata, aes(age, ring.width, group=tag)) +
  geom_point(color= "gray") +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", show.legend=TRUE)


rw_age<- read.csv("../data/model_coefficients/rw_age.csv")

ggplot(mdata, aes(age_s, ring.width_s)) +
  labs(x= "Scaled ring age", y= "Scaled ring width (cm)") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=rw_age, aes(slope=age,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  pubtheme.nogridlines


# Ring width by drought seperated by subsections

rw_PMDI <- read.csv("../data/model_coefficients/rw_PMDI.csv")

ggplot(mdata, aes(PMDI_3yrlag_s, ring.width_s)) +
  labs(x= "Scaled 3 year lagged drought values", y= " Scaled Ring Width (cm)") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=rw_PMDI, aes(slope=PMDI,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  pubtheme.nogridlines

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)





## Resin duct density model ##

# resin duct density by subsection
ggplot(mdata, aes(subsections, duct.density.log)) +
  labs(title="Resin duct density by Subsections", x= "Subsection", y= "Logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  pubtheme.nogridlines+
  geom_boxplot() 

# resin duct density by age and subsections

dd_age <- read.csv("../data/model_coefficients/dd_age.csv")

ggplot(mdata, aes(age_s, duct.density.log_s)) +
  labs(x= "Scaled ring age", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=dd_age, aes(slope=age,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  pubtheme.nogridlines

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80, 90, 100))),
       aes(age, duct.density.log, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)

# resin duct density and elevation by subsections

dd_elev <- read.csv("../data/model_coefficients/dd_elev.csv")

ggplot(unique.trees, aes(elev_s, duct.den.log.mean_s)) +
  labs(x= "Scaled elevation (meters)", y= "Scaled mean logged
       resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=dd_elev, aes(slope=elevation,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  pubtheme.nogridlines

# resin duct density and ring width by subsections

dd_rw<- read.csv("../data/model_coefficients/dd_rw.csv")

ggplot(mdata, aes(ring.width_s, duct.density.log_s)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=dd_rw, aes(slope=rw,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  # stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  # bestfit +
  pubtheme.nogridlines

ggplot(mdata, aes(ring.width, duct.density.log, group=tag)) +
  geom_point(color= "gray") +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)

ggplot(unique.trees, aes(ring.width.mean, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mutate(mdata, Age=cut(age, c(0,10,20,30,40, 50, 60, 70, 80, 90, 100))),
       aes(ring.width_s, duct.density.log_s, color=Age)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)+
  pubtheme.nogridlines

ggplot(mutate(mdata, Age=cut(age, c(0,10,20,30,40, 50, 60, 70, 80, 90, 100))),
       aes(ring.width_s, duct.density.log_s, color=Age)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE, inherit.aes = FALSE,data= filter(mdata, age <= 10), aes(ring.width, duct.density.log))+
  pubtheme.nogridlines

ggplot(mdata, aes(ring.width_s, duct.density.log_s, group=tag)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=dd_rw, aes(slope=rw,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  bestfit +
  pubtheme.nogridlines

### Interesting, the first 10 years show a negative relationship with ring
# ring width and duct density, but that changes after that especially with strobus.

ggplot(filter(mdata, age <= 10), aes(ring.width_s, duct.density.log_s)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  geom_abline(data=dd_rw, aes(slope=rw,intercept=intercept), color = "black", size=1.5) +
  facet_grid(. ~ subsections) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  bestfit +
  pubtheme.nogridlines

ggplot(filter(mdata, age >= 10), aes(ring.width_s, duct.density.log_s)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2) +
  facet_grid(. ~ subsections) +
  stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
  bestfit +
  pubtheme.nogridlines

ggplot(mdata, aes(ring.width, duct.density.log)) +
  labs(x= "Scaled ring width (cm)", y= "Scaled logged resin duct density") +
  geom_point(color= "gray", alpha=0.2, inherit.aes = FALSE, aes(ring.width.mean_minus10, duct.den.log.mean_minus10, data = unique.trees_10)) +
  facet_grid(. ~ subsections) +
  bestfit +
  pubtheme.nogridlines

unique.trees_10 <- mdata %>% group_by(tag, subsections, spcode) %>% filter(age <= 10) %>%
  dplyr::summarize(ring.width.mean_minus10 = mean(ring.width),
                   duct.den.log.mean_minus10 = mean(duct.density.log))

ggplot(unique.trees_10, aes(ring.width.mean_minus10, duct.den.log.mean_minus10)) +
  geom_point(color= "gray", alpha=0.2) +
               bestfit +
               pubtheme.nogridlines +
  facet_grid(.~subsections)

unique.trees_plus10 <- mdata %>% group_by(tag, subsections, spcode) %>% filter(age >= 10) %>%
  dplyr::summarize(ring.width.mean_plus10 = mean(ring.width),
                   duct.den.log.mean_plus10 = mean(duct.density.log, na.rm = TRUE))

ggplot(unique.trees_plus10, aes(ring.width.mean_plus10, duct.den.log.mean_plus10)) +
  geom_point(color= "gray", alpha=0.2) +
  bestfit +
  pubtheme.nogridlines +
  facet_grid(.~subsections)


ggplot() +
  labs(x= "Ring width (cm)", y= "Logged resin duct density") +
  geom_point(data= filter(mdata, age >= 10), aes(ring.width, duct.density.log),
             color= "gray", alpha=0.2) +
  geom_point(data= filter(mdata, age <= 10), aes(ring.width, duct.density.log),
             color= "tomato", alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE,data= filter(mdata, age >= 10),
              aes(ring.width, duct.density.log), color="black")+
  geom_smooth(method="lm", se=FALSE,data= filter(mdata, age <= 10),
              aes(ring.width, duct.density.log), color="red") +
  pubtheme.nogridlines
