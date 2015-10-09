## precip_data.R
## -------------
## Precipitation data from four weather stations

# Load programs
library(plyr)
library(stringr)

# Load .csv file from dropbox
monthly_precip_data <- read.csv("https://www.dropbox.com/s/iqi7e4n8sietwtu/precip_data.
                        csv?dl=1")
# Remove numbers for months in TPCP column
str_sub(monthly_precip_data$DATE, 5, -1) <- "";

# Summarize monthly precipitation averages to obtain yearly averages
yearly_precip_data <- (ddply(subset(monthly_precip_data), 
      .(STATION_NAME,YEAR= DATE),summarize, PRECIP = sum(TPCP)))

head(yearly_precip_data)

