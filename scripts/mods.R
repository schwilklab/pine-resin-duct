
library(nlme)


# read data, only select complete cases so no NA's exist
mdata <- ring_data[complete.cases(ring_data), ]
# remove the first year of growth since no resin ducts are present in pith
mdata <- filter(mdata, !(tree.age==1))
# remove last year of data since these are only partial growth years
mdata <- filter(mdata, !(calendar.year==2015))
                    
full.mod <- lme(duct.density ~ tree.age + BAF + elev + regional_precip + radiation
                + PMDI + slope + spcode + bai + spcode:tree.age + mtn,
                random = ~ 1 | tag, data=mdata, method="ML")
full.null <-  lme(duct.density ~ 1 , random = ~ 1 | tag, data=mdata, method="ML")