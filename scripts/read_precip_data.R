## read_precip_data.R
## -------------
## Precipitation data from four weather stations.  Code extracts the 
## values and creates a new data set obtaining the precipitation 
## amount present at each station per year.  Values will be used to
## populate values into dataset in rings.R.

# Load programs
library(plyr)
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
yearly_precip_data <- (ddply(monthly_precip_data, .(STATION_NAME, calendar.year), summarize, PRECIP = sum(TPCP)))
head(yearly_precip_data)

# Create new dataframe to merge into yearly_precip_data
station_names<- read.csv("../data/station_names.csv")

# Merge values associated with each station into yearly_precip_data
yearly_precip_data <- left_join(yearly_precip_data, station_names, by = "STATION_NAME")

# Remove station_names
rm(station_names)

# Quick plot to see precipitation trends for each range
ggplot(yearly_precip_data, aes(calendar.year, PRECIP)) +
           geom_line() +
           facet_grid(STATION_NAME ~ .)
