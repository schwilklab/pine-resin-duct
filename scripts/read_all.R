# Read and clean all data

# -"trees.sum" data frame is added to the global namespace
# -"mdata" data frame is added to the global namespace and is used as the
#   dataframe for models.

library(dplyr)

## Read and clean individual tree data. Tag is unique row identifier.
trees <- read.csv("../data/trees.csv", stringsAsFactors=FALSE)
species <- read.csv("../data/species.csv", stringsAsFactors=FALSE)

## Read topographic data for assigning environmental variables to tagged trees.
# Run read_raster_data.R to create these RDS files
# source("read_raster_data.R")
cm_raster_data <- readRDS(file="../results/cm_raster_data.rds")
dm_raster_data <- readRDS(file="../results/dm_raster_data.rds")
gm_raster_data <- readRDS(file="../results/gm_raster_data.rds")

# Create new data frame with raster info on all trees as well as other 
# variables measured and calculated
trees <- trees %>% mutate(gps.elev=elev) %>% dplyr::select(-lat, -lon, -elev) %>%
  inner_join(bind_rows(cm_raster_data, dm_raster_data, gm_raster_data))

# Get all annual ring calculations
source("read_rings.R")

# get annual PMDI data
source("read_climate.R")

# Add drought values to the data frame.
# Create new data frames to be used for the models and for graphs.
mdata <- left_join(ring_data, yearly_drought, by= "calendar.year")

# transforms
mdata <- mdata %>% mutate(duct.per.circ = resin.duct.count / ((r2)^2*pi),
                                        duct.density.log = log(duct.density+10),
                                        bai.log = log(bai+0.0001),
                                        fyear = as.factor(calendar.year))




# Select complete cases so no NA's exist
mdata <- mdata[complete.cases(mdata), ] %>%
  filter(age!=1) %>% # remove the first year of growth since no resin
  # ducts are present in pith remove last year of
  # data since these are only partial growth years
  filter(calendar.year != 2015) 


# Rescale numeric variables
zscore <- function(x) (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)  
mdata <- mdata %>% mutate_each(funs(s = zscore(.)), -tag, -spcode, -mtn, -date, 
                               -fyear, -subsection, -species_name)

## mdata <- mdata %>% mutate(subsection = factor(subsection),
##                           spcode = factor(spcode),
##                           tag = factor(tag))

# Calculate summaries per tree

# Calculate tree age first using ring_data since that retains all years
trees.sum <- mdata %>% group_by(tag) %>%
  dplyr::summarize(avg.age = mean(age),
                   age.sd = sd(age),
                   age.min = min(age), max.age = max(age))

# Next perform summary calculations and combine all data together
trees.sum <- mdata %>% group_by(tag) %>%
  dplyr::summarize(duct.count.mean = mean(resin.duct.count, na.rm = TRUE),
                   duct.count.sd = sd(resin.duct.count, na.rm = TRUE),
                   duct.den.mean = mean(duct.density, na.rm = TRUE),
                   duct.den.sd = sd(duct.density, na.rm = TRUE),
                   duct.den.log.mean = mean(duct.density.log, na.rm = TRUE),
                   duct.den.log.sd = sd(duct.density.log, na.rm = TRUE),
                   total.duct.count.mean = mean(total.duct.count, na.rm = TRUE),
                   total.duct.count.sd = sd(total.duct.count, na.rm = TRUE),
                   ring.width.mean = mean(ring.width),
                   ring.width.sd = sd(ring.width),
                   bai.mean = mean(bai),
                   bai.sd = sd(bai),
                   PMDI.mean = mean(PMDI_3yrlag, na.rm = TRUE),
                   PMDI.sd = sd(PMDI_3yrlag, na.rm = TRUE)
                   ) %>% inner_join(trees) %>%
                   left_join(trees.sum, by= "tag") %>%
                   left_join(species, by= "spcode") %>%
                   select(-core.taken, -pith, -needles.collected, -condition,
                          -barkbeetle.attack, -trail.area, -note)

