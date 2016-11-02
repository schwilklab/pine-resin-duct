# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

library(ggplot2)
library(lme4)
source("read_rings.R")

# Create dataframe with information unique to each tree
unique.trees <- mdata %>% group_by(tag) %>% top_n(1, age)


## Ring width model ##

## Ring width by subsection, accounting for age and drought
age_pmdi_mod <- lmer(ring.width ~ age + PMDI_3yrlag + age:PMDI_3yrlag +
                       (age_s + PMDI_3yrlag_s | tag) + (1 | calendar.year), 
                        data=mdata, REML=FALSE)
mdata$rw_age_pmdi_resid <- residuals(age_pmdi_mod)
rw_subs_df <- mdata %>% group_by(tag, subsections) %>%
  summarize(rw_resid= mean(rw_age_pmdi_resid))
ggplot(rw_subs_df, aes(subsections, rw_resid)) + geom_boxplot() +
  xlab("Subsection") + ylab("Mean residual ring width per tree")
# So not terribly dramatic. But This does not quite match conclusion from your
# models that strobus is higher growth. I'm not sure I believe the model then.
# Do you models have two many random effects? Double check.


## Effect of age by subsection
# center ages by subsection so we can make use of interecept as well as slope
age_df <- mdata %>% group_by(subsections) %>% mutate(age_c = age - mean(age))
getcoefs <- function(df) {
  m <- lm(ring.width ~ age_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2]))
}
age_mods <- age_df %>% group_by(subsections, spcode, tag, mtn) %>% do(getcoefs(.))
ggplot(age_mods, aes(subsections, s)) + geom_boxplot()
## rw tends to decrease with age. Slight tendency for this to happen more
## rapidly in ponderosae but driven by a few individual trees. Lets look:
ggplot(age_mods, aes(subsections, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn)
# Yes, driven intirely by some of the PIPO, not by PIAR5 Maybe check which
# range those are in. Those outliers are a problem

# Ring width by drought across subsections
age_mod <- lmer(ring.width ~ age_s + (1 + age_s | tag), data=mdata, REML=FALSE)
mdata$rw_age_resid <- residuals(age_mod)
rw_year_df <- mdata %>% group_by(subsections, calendar.year) %>%
  summarize(rw_resid = mean(rw_age_resid), pmdi= mean(PMDI_3yrlag))

# figure with each point a calendar year
ggplot(rw_year_df, aes(pmdi, rw_resid, color=subsections)) + geom_point() +
  geom_smooth(method="lm", se=FALSE) + ylab("Residual ring width after accounting for tree age")
# Strobus most sensitive to pmdi These residuals are after ccounting for age
# effects with separate slopes and intercepts per tree. Altenratiely, one could
# run an overal age model, eg lm(ring.width ~ age_s, data=mdata)

########################################
# resin duct density

# Resin duct density by subsection accounting for all other factors
density_mod <- lmer(duct.density.log_s ~ age_s + elev_s + ring.width_s + age_s:ring.width_s +
                                (age_s+PMDI_3yrlag_s | tag) + (1 | calendar.year),
                              data=mdata, REML=FALSE)
mdata$age_elev_rw_resid <- residuals(density_mod)
dd_subs_df <- mdata %>% group_by(tag, subsections) %>%
  summarize(ducts_resid= mean(age_elev_rw_resid))
ggplot(dd_subs_df, aes(subsections, ducts_resid)) + geom_boxplot() +
  xlab("Subsection") + ylab("Mean residual logged duct density per tree")
# So Cembroides truely does have the larger resin duct density values

# Effect of age on resin duct density
ductdensity_df <- mdata %>% group_by(subsections) %>% mutate(age_c = age - mean(age))
getcoefs_dd <- function(df) {
  m <- lm(duct.density.log ~ age_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2]))
}

duct_mods <- ductdensity_df %>% group_by(subsections, spcode, tag, mtn) %>% do(getcoefs_dd(.))
ggplot(duct_mods, aes(subsections, s)) + geom_boxplot() +
  xlab("Subsection") + ylab("Age coefficients with resin duct
                            density as response variable")
# So it does look like Strobus does decrease at a greater rate than the other two subsections, 
# but it's not by a large ammount.

ggplot(duct_mods, aes(subsections, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn) + xlab("Subsection") + ylab("Age coefficients with resin duct
                                                 density as response variable")
 # Looks like there are two big outliers for P. ponderosa in the GM


# Effect of ring width on resin duct density
ductdensity_rw_df <- mdata %>% group_by(subsections) %>% mutate(ring.width_c = ring.width - mean(ring.width))
getcoefs_dd_rw <- function(df) {
  m <- lmer(duct.density.log ~ ring.width_c, data=df)
  return(data.frame(i = coef(m)[1], s = coef(m)[2]))
}

duct_mods_rw <- ductdensity_rw_df %>% group_by(subsections, spcode, tag, mtn) %>% do(getcoefs_dd_rw(.))
ggplot(duct_mods_rw, aes(subsections, s)) + geom_boxplot()  + xlab("Subsection") +
ylab("Ring width coefficients with resin duct
      density as response variable")
## It looks like resin duct density does increase with ring width in strobus
ggplot(duct_mods_rw, aes(subsections, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn) + xlab("Subsection") + ylab("Ring width coefficients with resin duct
                                                 density as response variable")
## It looks like each species is behaving similarily according to their subsection.  There is 
## one value in arizonica that is really low, though.










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
