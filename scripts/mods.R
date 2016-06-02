
library(nlme)


# read data
mdata <- ring_data[complete.cases(ring_data), ]
                    
full.mod <- lme(duct.density ~ age + BAF + elev + slope + spcode + spcode:age,
                random = ~ 1 | tag, data=mdata, method="ML")
full.null <-  lme(duct.density ~ 1 , random = ~ 1 | tag, data=mdata, method="ML")
