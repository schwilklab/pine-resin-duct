# mods.R

# Setting up models to determine the best model that explains what
# is happening with the data. Three different types of models are 
# produced.  Models looking at resin duct density, count, and bai.
# For each dependent variable analyzed, a full model is produced
# which includes all variables, as well as seperate models which
# include only the indvidual varaibles by themselves to get an 
# idea on the relative influence they have as a predictor. Lastly,
# a null model is produced to compare to.

library(lme4)
library(MuMIn)

# read data, only select complete cases so no NA's exist
mdata <- ring_data[complete.cases(ring_data), ]
# remove the first year of growth since no resin ducts are present in pith
mdata <- filter(mdata, !(age==1))
# remove last year of data since these are only partial growth years
mdata <- filter(mdata, !(calendar.year==2015))


###### 1. Resin Duct Density ##########

# Create linear models for resin duct density.  Show a full model with 
# all predicted variables, and seperate models showing each variable by itself
# to show relative influence of each variable.  Also created a null model to 
# compare to.

full.duct.mod <- lmer(duct.density ~ age + BAF + elev + radiation
                 + PMDI_3yrlag + spcode:PMDI_3yrlag + slope +
                   spcode + bai + spcode:age + mtn +(1|tag) +
                   (1|calendar.year), data=mdata, REML=FALSE)

topo.duct.mod <- lmer(duct.density ~ elev + radiation + slope + (1|tag) +
                      (1|calendar.year), data=mdata, REML=FALSE)

age.duct.mod <- lmer(duct.density ~ age + (1|tag) +
                     (1|calendar.year), data=mdata, REML=FALSE)

drought.duct.mod <- lmer(duct.density ~ PMDI_3yrlag + (1|tag) +
                         (1|calendar.year), data=mdata, REML=FALSE)

species.duct.mod <- lmer(duct.density ~ spcode + (1|tag) +
                         (1|calendar.year), data=mdata, REML=FALSE)

bai.duct.mod <- lmer(duct.density ~ bai + (1|tag) +
                     (1|calendar.year), data=mdata, REML=FALSE)

baf.duct.mod <- lmer(duct.density ~ BAF + (1|tag) +
                     (1|calendar.year), data=mdata, REML=FALSE)

mtn.duct.mod <- lmer(duct.density ~ mtn + (1|tag) +
                     (1|calendar.year), data=mdata, REML=FALSE)

full.duct.null <-  lmer(duct.density ~ 1 + (1|tag) + (1|calendar.year),
                        data=mdata, REML=FALSE)

summary(full.duct.mod)
anova(full.duct.mod)
anova(full.duct.mod, topo.duct.mod, age.duct.mod, drought.duct.mod,
      species.duct.mod, bai.duct.mod, baf.duct.mod, mtn.duct.mod, full.duct.null)


###### 2. Resin Duct Count ##########

full.count.mod <- lmer(resin.duct.count ~ age + BAF + elev + radiation
                      + PMDI_3yrlag + spcode:PMDI_3yrlag + slope +
                        spcode + bai + spcode:age + mtn +(1|tag) +
                        (1|calendar.year), data=mdata, REML=FALSE)

topo.count.mod <- lmer(resin.duct.count ~ elev + radiation + slope + (1|tag) +
                        (1|calendar.year), data=mdata, REML=FALSE)

age.count.mod <- lmer(resin.duct.count ~ age + (1|tag) +
                       (1|calendar.year), data=mdata, REML=FALSE)

drought.count.mod <- lmer(resin.duct.count ~ PMDI_3yrlag + (1|tag) +
                           (1|calendar.year), data=mdata, REML=FALSE)

species.count.mod <- lmer(resin.duct.count ~ spcode + (1|tag) +
                           (1|calendar.year), data=mdata, REML=FALSE)

bai.count.mod <- lmer(resin.duct.count ~ bai + (1|tag) +
                       (1|calendar.year), data=mdata, REML=FALSE)

baf.count.mod <- lmer(resin.duct.count ~ BAF + (1|tag) +
                       (1|calendar.year), data=mdata, REML=FALSE)

mtn.count.mod <- lmer(resin.duct.count ~ mtn + (1|tag) +
                       (1|calendar.year), data=mdata, REML=FALSE)

full.count.null <-  lmer(resin.duct.count ~ 1 + (1|tag) + (1|calendar.year),
                        data=mdata, REML=FALSE)

summary(full.count.mod)
anova(full.count.mod)
anova(full.count.mod, topo.count.mod, age.count.mod, drought.count.mod,
      species.count.mod, bai.count.mod, baf.count.mod, mtn.count.mod, full.count.null)


###### 3. Basal Area Index ##########

full.bai.mod <- lmer(bai ~ age + BAF + elev + radiation
                       + PMDI_3yrlag + spcode:PMDI_3yrlag + slope +
                         spcode + spcode:age + mtn +(1|tag) +
                         (1|calendar.year), data=mdata, REML=FALSE)

topo.bai.mod <- lmer(bai ~ elev + radiation + slope + (1|tag) +
                     (1|calendar.year), data=mdata, REML=FALSE)

age.bai.mod <- lmer(bai ~ age + (1|tag) + (1|calendar.year), data=mdata, REML=FALSE)

drought.bai.mod <- lmer(bai ~ PMDI_3yrlag + (1|tag) + (1|calendar.year),
                        data=mdata, REML=FALSE)

species.bai.mod <- lmer(bai ~ spcode + (1|tag) + (1|calendar.year),
                        data=mdata, REML=FALSE)

baf.bai.mod <- lmer(bai ~ BAF + (1|tag) + (1|calendar.year), data=mdata, REML=FALSE)

mtn.bai.mod <- lmer(bai ~ mtn + (1|tag) + (1|calendar.year), data=mdata, REML=FALSE)

full.bai.null <-  lmer(bai ~ 1 + (1|tag) + (1|calendar.year), data=mdata, REML=FALSE)

summary(full.bai.mod)
anova(full.bai.mod)
anova(full.bai.mod, topo.bai.mod, age.bai.mod, drought.bai.mod,
      species.bai.mod, baf.bai.mod, mtn.bai.mod, full.bai.null)

