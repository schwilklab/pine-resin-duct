## read_precip_data.R
## -------------
## Precipitation data from multiple weather stations.  Code extracts the 
## values and creates a new data set obtaining the precipitation 
## amount present at each station per year.  Values will be used to
## populate values into dataset in rings.R.

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

# Load programs
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sp)

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
monthly_precip_data <- read.csv("https://www.dropbox.com/s/iqi7e4n8sietwtu/precip_data.csv?dl=1")
# Remove numbers for months in TPCP column
str_sub(monthly_precip_data$DATE, 5, -1) <- "";
monthly_precip_data$calendar.year <- as.integer(str_sub(monthly_precip_data$DATE, 1, 4))

# Summarize monthly precipitation averages to obtain yearly averages
yearly_precip_data1 <- (ddply(monthly_precip_data, .(STATION_NAME, calendar.year), summarize, PRECIP = sum(TPCP), num_mnths = length(calendar.year)))
head(yearly_precip_data1)

# Create new dataframe to merge into yearly_precip_data
station_names<- read.csv("../data/station_names.csv")

# Convert integers into character
station_names$STATION_NAME<- as.character(station_names$STATION_NAME)
station_names$mtn<- as.character(station_names$mtn)

# STATION_NAME is a character in station_names df, but a factor in
# yearly_precip_data1.  This will cause problems for joining, so here
# is some code to work around that.
combined <- sort(union(levels(yearly_precip_data1$STATION_NAME),
                       levels(station_names$STATION_NAME)))

# Merge values associated with each station into yearly_precip_data
yearly_precip_data <- left_join(mutate(yearly_precip_data1, STATION_NAME=factor(STATION_NAME, levels=combined)),
                                mutate(station_names, STATION_NAME=factor(STATION_NAME, levels=combined)))

# Only select years that have 12 months of data
yearly_precip_data<- filter(yearly_precip_data, num_mnths==12)
# Remove station_names ad other temporary values
rm(station_names, yearly_precip_data1, combined)

# Quick plot to see precipitation trends for each range
ggplot(yearly_precip_data, aes(calendar.year, PRECIP, color=STATION_NAME)) +
           geom_line()+
           facet_grid(mtn ~ .)

# Palmer drought values for Trans Pecos region. Values are specific to
# Trans-Pecos area defined by the boundaries observed by NOAA.  Link
# to the map of the encolsed area is: http://gis.ncdc.noaa.gov/map/viewer/#app=cdo&cfg=cdo&theme=indices&layers=01&node=gis
# Data obtained from http://www7.ncdc.noaa.gov/CDO/CDODivisionalSelect.jsp#)

# Read .csv file
monthly_drought_values<- read.csv("../data/drought_values_transpecos.csv")
# Remove month from dataframe
str_sub(monthly_drought_values$YearMonth, 5, -1) <- "";
# Create a new column with calendar.year to be used to merge into ring_data
# from script in read_rings.R
monthly_drought_values$calendar.year <- as.integer(str_sub(drought_values$YearMonth, 1, 4))
# Combine monthly values and summarize by mean for regional precipitation,
# also covert precip values to metric.  Calculates the average Palmer Drought
# Severity Index and Modified version as well.
yearly_drought_values <- (ddply(monthly_drought_values, .(calendar.year), summarize, regional_precip = (sum(PCP)*2.54),
                                PDSI = mean(PDSI), PMDI= mean(PMDI)))

ggplot(yearly_drought_values, aes(calendar.year, PDSI)) +
  geom_line()
