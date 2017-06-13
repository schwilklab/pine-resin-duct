## read_precip_drought_data.R
## -------------
## Precipitation data from multiple weather stations.  Code extracts the 
## values and creates a new data set obtaining the precipitation 
## amount present at each station per year.  Values will be used to
## populate values into dataset in rings.R. Also added regional 
## drought data. See ring_data metadata file for explanation of columns
## present in the data frame.

# What is produced:
#
# -"deg2rad" function which converts degrees to radians
# -"gcd.hf" function which calculates the distance between two sets of 
#   coordinates
# 
# -"monthly_precip_data" data frame which is used to calculate yearly values
# -"yearly_precip_data" data frame which contains annual precipitation rates
#   for each station that has 12 months of data
# -"monthly_drought_values" data frame which is used to calculate yearly values
# -"yearly_drought_values" data frame which contains annual precipitation values
#   for the Trans Pecos boundary as defined by NOAA.  Also contains Palmer
#   drought severity index values and modified Palmer drought severity index 
#   values averaged by monthly values.

# Load packages
library(dplyr)
library(stringr)
#library(ggplot2)

# Function to convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified by
# radian latitude/longitude using the Haversine formula (hf)

gcd.hf <- function(long1, lat1, long2, lat2) {
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

# Load .csv file from dropbox
monthly_precip <- read.csv("../data/precip_data.csv",
                           stringsAsFactors=FALSE) %>% mutate(YearMonth = DATE)
monthly_precip <- monthly_precip %>% mutate(DATE=as.character(DATE),
                                            calendar.year = as.integer(str_sub(DATE, 1, 4)),
                                            month = as.integer(str_sub(DATE, 5, -1)))

# Summarize monthly precipitation averages to obtain yearly averages
yearly_precip <- monthly_precip %>% group_by(STATION_NAME, calendar.year) %>%
  summarize( PRECIP = sum(TPCP), num_months = length(calendar.year))

# Create new dataframe to merge into yearly_precip_data
stations <- read.csv("../data/wx_stations.csv", stringsAsFactors=FALSE)

# Merge values associated with each station into yearly_precip data
yearly_precip <- left_join(yearly_precip, stations)

# Only select years that have 12 months of data
yearly_precip <- filter(yearly_precip, num_months==12)

# Remove station_names ad other temporary values
rm(stations, monthly_precip)

# Quick plot to see precipitation trends for each range
## ggplot(yearly_precip, aes(calendar.year, PRECIP, color=STATION_NAME)) +
##   geom_line()+
##   facet_grid(mtn ~ .)


# Palmer drought values for Trans Pecos region. Values are specific to
# Trans-Pecos area defined by the boundaries observed by NOAA.  Link
# to the map of the encolsed area is: http://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=indices&layers=01&node=gis
# Data obtained from http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp#)
# Further explanation for drought values is located here: http://www.ncdc.noaa.gov/temp-and-precip/drought/historical-palmers/overview

# Read .csv file
monthly_drought <- read.csv("../data/drought_values_transpecos.csv", stringsAsFactors=FALSE)
monthly_drought <- monthly_drought %>% mutate(YearMonth=as.character(YearMonth),
                                            calendar.year = as.integer(str_sub(YearMonth, 1, 4)),
                                            month = as.integer(str_sub(YearMonth, 5, -1)))


# Combine monthly values and summarize by mean for regional precipitation, also
# covert precip values to metric. Calculates the average Palmer Drought
# Severity Index and Modified version as well. Lastly, creates a column with
# the mean of that years PMDI and the two preceeding years values. Rows with
# NA's produced are removed afterwardd.
yearly_drought <- monthly_drought %>% group_by(calendar.year) %>%
  summarize(regional_precip = (sum(PCP)*2.54),
            PDSI = mean(PDSI), PMDI= mean(PMDI)) %>%
  mutate(PMDI_3yrlag = (PMDI + lag(PMDI) +lag(PMDI, 2)) / 3)

## ggplot(yearly_drought, aes(calendar.year, PMDI)) +
##    geom_line()

