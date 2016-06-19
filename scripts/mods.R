# mods.R

# Setting up models to determine the best model that explains what
# is happening with the data.

library(nlme)


# read data, only select complete cases so no NA's exist
mdata <- ring_data[complete.cases(ring_data), ]
# remove the first year of growth since no resin ducts are present in pith
mdata <- filter(mdata, !(ring==1))
# remove last year of data since these are only partial growth years
mdata <- filter(mdata, !(calendar.year==2015))

# Create data frame that includes all rings that have resin duct densities
# associated with the ring.  This adds a few hundred more data points than
# what is seen with mdata.  This is used to look at a species effect. 

# First line removes the first rings data because that refers to the pith where
# resin ducts aren't present.  Second line removes the last year of growth
# since that contains partial data because it was sampled before the growth
# year ended. Last line removes any remaining NA values.

all_resin_data<- ring_data %>% filter(!(ring==1)) %>%
  filter(!(calendar.year==2015)) %>%
  filter(!(is.na(duct.density)))

# Look at the mean resin duct densities with their standard error of mean
# across the five species.
ddply(subset(all_resin_data), 
      .(spcode), summarize, mean.duct.den = mean(duct.density),
      se.duct.den = mean(sd(duct.density)/length(ring)))

# This is interesting, ponderosa pines on average produced about half
# of the resin ducts per the same area as did pinyon pines.  

# Do a linear model on this.
resinduct.lm <- lm(duct.density ~ spcode, data=all_resin_data)
summary(resinduct.lm)
anova(resinduct.lm)
# Suggests a signficant difference in resin duct density across all species.
                    
# Now set up for full model using all varables

# NEED TO SOLVE: Assumes incorrect degrees of freedom for a lot of
# variables.  Specifcally, regional_precip and PMDI, 
full.mod <- lme(duct.density ~ ring + BAF + elev + regional_precip + radiation
                + PMDI_3yrlag + slope + spcode + bai + spcode:ring + mtn,
                random = ~ 1 | tag, data=mdata, method="ML")
full.null <-  lme(duct.density ~ 1 , random = ~ 1 | tag, data=mdata, method="ML")

summary(full.mod)

anova(full.mod, full.null)

