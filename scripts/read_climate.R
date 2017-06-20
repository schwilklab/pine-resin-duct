## read_climate.R
## -------------
## Read NOAA monthly precip values and calculate
## Palmer Drought Severity index for Trans-Pecos region.

# What is produced:
#
# -"monthly_drought_values" data frame which is used to calculate yearly values
# -"yearly_drought_values" data frame which contains annual precipitation values
#   for the Trans Pecos boundary as defined by NOAA.  Also contains Palmer
#   drought severity index values and modified Palmer drought severity index 
#   values averaged by monthly values.

library(dplyr)
library(stringr)

# Palmer drought values for Trans Pecos region. Values are specific to
# Trans-Pecos area defined by NOAA. Link to the map of the encolsed area is:
# http://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=indices&layers=01&node=gis
# Data obtained from http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp#)
# Further explanation for drought values is located here:
# http://www.ncdc.noaa.gov/temp-and-precip/drought/historical-palmers/overview

# Read .csv file
monthly_drought <- read.csv("../data/drought_values_transpecos.csv", stringsAsFactors=FALSE)
monthly_drought <- monthly_drought %>% mutate(YearMonth=as.character(YearMonth),
                                            calendar.year = as.integer(str_sub(YearMonth, 1, 4)),
                                            month = as.integer(str_sub(YearMonth, 5, -1)))

# Combine monthly values and summarize by mean for regional precipitation, also
# covert precip values to metric. Calculates the average Palmer Drought
# Severity Index and Modified version as well. Lastly, creates a column with
# the mean of that years PMDI and the two preceeding years values. Rows with
# NA's are removed afterward.
yearly_drought <- monthly_drought %>% group_by(calendar.year) %>%
  summarize(regional_precip = (sum(PCP)*2.54),
            PDSI = mean(PDSI), PMDI= mean(PMDI)) %>%
  mutate(PMDI_3yrlag = (PMDI + lag(PMDI) +lag(PMDI, 2)) / 3)



## Visualize
## ggplot(yearly_drought, aes(calendar.year, PMDI)) +
##    geom_line()

