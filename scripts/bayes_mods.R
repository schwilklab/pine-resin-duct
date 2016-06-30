#!/usr/bin/Rscript

# Bayesian mixed models.  See mods.R for lme4 absed exploration

library(brms)

source("read_rings.R")

# read data, only select complete cases so no NA's exist
mdata <- ring_data[complete.cases(ring_data), ] %>%
  filter(age!=1) %>% # remove the first year of growth since no resin
                              # ducts are present in pith remove last year of
                              # data since these are only partial growth years
  filter(calendar.year != 2015) 

# transforms
mdata <- mdata %>% mutate(duct.per.circ = resin.duct.count / 2*r2*pi,
                          duct.density.log = log(duct.density+1),
                          fyear = as.factor(calendar.year))
## Rescale numeric variables ##
mdata <- mdata %>% mutate_each(funs(s = scale(.)), -tag, -spcode, -mtn, -date, -fyear)


# ring width

## # This takes about 45 min to run 3 chains!
## b.growth.mod1 <- brm(ring.width_s ~ spcode*(age_s + PMDI_3yrlag_s + BAF_s + elev_s) +
##                        (age_s+PMDI_3yrlag_s | tag) + (1 | fyear), data=mdata,
##                      chains=3, iter=1200)

## saveRDS(b.growth.mod1, "../results/b_gorwth_mod1.rds")



b.count.mod1 <- brm(resin.duct.count ~  spcode*(ring.area_s + age_s + BAF_s +
                                                  PMDI_3yrlag_s + elev_s) +
                      (age_s+PMDI_3yrlag_s | tag) + (1 | fyear), data=mdata, family="poisson", chains = 3, iter = 2000)

saveRDS(b.count.mod1, "../results/b_count_mod1.rds")

##                            elev_s +
##                            (age | tag) +
##                            (1 | calendar.year),
  ##                          data=mdata, family=poisson)

  
