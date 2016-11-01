# lmer_mods_graphs.r

# This R script contains graphs representing some of the significant
# relationships expressed by models created in lmer_mods.R.

library(ggplot2)
library(lme4)
source("read_rings.R")

# Create dataframe with information unique to each tree
unique.trees <- mdata %>% group_by(tag) %>% top_n(1, age)


## Ring width model ##

# Ring width by subsections 
ggplot(mdata, aes(subsections, ring.width.mean)) +
  geom_violin() 

ggplot(mdata, aes(subsections, ring.width)) +
  geom_boxplot()

ggplot(unique.trees, aes(subsections, ring.width.mean))+
  geom_violin()


## Ring width by subsection, accounting for age and drought
age_pmdi_mod <- lmer(ring.width ~ age + PMDI_3yrlag + age:PMDI_3yrlag +
                       (1 | calendar.year), 
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
age_mods <- age_df %>% group_by(subsections, spcode, tag) %>% do(getcoefs(.))
ggplot(age_mods, aes(subsections, s)) + geom_boxplot()
## rw tends to decrease with age. Slight tendency for this to happen more
## rapidly in ponderosae but driven by a few individual trees. Lets look:
ggplot(age_mods, aes(subsections, s, color=spcode)) + geom_point(position="jitter")
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
# effects with separate slopes and intercepts per tree. ALtenratiely, one could
# run an overal age model, eg lm(ring.width ~ age_s, data=mdata)


#### Erik's :
ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  geom_smooth(method="lm", se=FALSE)

# ring width increases at all points in time of the tree's life as 
# drought conditions decrease (PMDI increases).

# Ring width by age seperated by subsections
ggplot(mdata, aes(age, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mdata, aes(age, ring.width)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")


# Ring width by drought seperated by subsections
ggplot(mdata, aes(PMDI_3yrlag_s, ring.width, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(PMDI_3yrlag, ring.width, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)





## Resin duct density model ##

# resin duct density by subsection
ggplot(mdata, aes(subsections, duct.density.log)) +
  geom_boxplot() 


# resin duct density by age

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  geom_smooth(method="lm")

# resin duct density by age and subsections

ggplot(mdata, aes(age, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

# resin duct density and elevation by subsections

ggplot(unique.trees, aes(elev, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections)+
  geom_smooth(method= lm)

# resin duct density and ring width by subsections

ggplot(mdata, aes(ring.width, duct.density.log, group=tag)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(unique.trees, aes(ring.width.mean, duct.den.log.mean)) +
  geom_point() +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm")

ggplot(mutate(mdata, fage=cut(age, c(0,10,20,30,40, 50, 60, 70, 80))),
       aes(ring.width, duct.density.log, color=fage)) +
  geom_point(alpha=0.2) +
  facet_grid(. ~ subsections) +
  geom_smooth(method="lm", se=FALSE)

### Interesting, the first 10 years show a negative relationship with ring
# ring width and duct density, but that changes after that especially with strobus.
