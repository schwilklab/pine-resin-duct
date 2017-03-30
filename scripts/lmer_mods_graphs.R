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

####### Figure 1: Drought and Ring Width #########

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
  ylab("Residual ring width") +
  xlab("PMDI")+
  pubtheme.nogridlines
# Strobus most sensitive to pmdi These residuals are after ccounting for age
# effects with separate slopes and intercepts per tree. Alternatively, one could
# run an overall age model, eg lm(ring.width ~ age_s, data=mdata)

########################################
# resin duct density

####### Figure 2: Influence of Age on Resin Duct Density #########

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
  ylab("Slope of correlation between \n resin duct density and age") +
  annotate("text", x = 1, y = .07, label = "a") +
  annotate("text", x = 2, y = .07, label = "a") +
  annotate("text", x = 3, y = .07, label = "b") +
  pubtheme.nogridlines
# So it does look like Strobus does decrease at a greater rate than the other two subsections, 
# but it's not by a large amount.

ggplot(duct_mods, aes(subsections, s, color=spcode)) + geom_point(position="jitter") +
  facet_grid(.~mtn) + xlab("Subsection") +
  ylab("Age coefficients with resin duct density as response variable") +
  pubtheme.nogridlines

####### Figure 3: Ring width and age on resin duct density #########

# function to assign young and old values to rings
factor_age<- function(age) {
  if (age <= 10){
    return ("Young")
  }
  else
    return ("Mature")
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
  scale_x_discrete(limits=c("Young", "Mature")) +
  facet_grid(.~subsections) +
  ylab("Slope of ring width and \n resin duct density correlation") +
  xlab("") +
  pubtheme.nogridlines


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
  annotate("text", x = 1, y = .11, label = "a") +
  annotate("text", x = 2, y = .11, label = "b") +
  annotate("text", x = 3, y = .11, label = "b") +
  xlab("Subsection") +
  ylab("Mean residual scaled \n resin duct density") +
  pubtheme.nogridlines
# So Cembroides truely does have the larger resin duct density values