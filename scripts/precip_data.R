## precip_data.R
## -------------
## Precipitation data from four weather stations

# Load programs
library(plyr)
library(stringr)
library(ggplot2)

# Load .csv file from dropbox
monthly_precip_data <- read.csv("https://www.dropbox.com/s/iqi7e4n8sietwtu/precip_data.
                        csv?dl=1")
# Remove numbers for months in TPCP column
str_sub(monthly_precip_data$DATE, 5, -1) <- "";
monthly_precip_data$year <- as.integer(str_sub(monthly_precip_data$DATE, 1, 4))

# Summarize monthly precipitation averages to obtain yearly averages
yearly_precip_data <- (ddply(monthly_precip_data,
                       .(STATION_NAME, year), summarize, PRECIP = sum(TPCP)))
head(yearly_precip_data)

ggplot(yearly_precip_data, aes(year, PRECIP)) +
           geom_line() +
           facet_grid(STATION_NAME ~ .)

