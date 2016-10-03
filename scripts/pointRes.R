# Runs code from the pointRes package to look at relative growth change,
# and identify potential pointer years where growth was significantly 
# different from previous years growth.

library(pointRes)
library(dplR)
library(tidyr)

# Subset data to create a data frame with each column containing the
# tree tag and each column corresponding with the ring width for 
# each year.
ring.width_data<-spread((subset(ring_data,select=c("tag", "calendar.year", "ring.width"))), tag, ring.width)
# Set row names to calendar year
rownames(ring.width_data) = ring.width_data$calendar.year[1]: 
  ring.width_data$calendar.year[length(ring.width_data$calendar.year)]
# Remove calendar.year column from data frame
ring.width_data$calendar.year<- NULL

# Normalization of data by year for skeleton plotting.  Uses Cropper 
# values (C), which reflect the number of standard deviations from the 
# local mean, which are used to identify event years (e.g. |C| > .75 SD)
# Then uses the info to identify negative and postive years.
pyc <- pointer.norm(ring.width_data, method.thresh = "Cropper")

# Distinguishes three intensity classes for event and pointer years,
# rather than identifying just positive or negative.
pyn <- pointer.norm(ring.width_data, method.thresh = "Neuwirth")

# Relative growth change
rgc<- pointer.rgc(ring.width_data)
# plot deviation of mean growth per year based on information from
# previous 4 years growth of each tree.  This lumps all trees together.
rgc.plot(rgc)

# Plot the output from pyn to see years where tree ring growth varied
# from the established norm.
event.plot(pyn)

# Calculates resilience components (resistance, recovery, resilience,
# and relative resilience).  This identifies low growth years and looks
# at how the following 4 years growth was affected.
res <- res.comp(ring.width_data)

# Plots the years that were identified as negative pointer years, and
# shows the indexes of the four variables mentioned earlier.  If there
# are less than 5 occurances, they will not be included in the plot.
res.plot(res)

resin.duct.count_data<- spread((subset(ring_data, select=c("tag", "calendar.year", "resin.duct.count"))),
           tag, ring.width)
ring.area_data<- spread((subset(ring_data, select=c("tag", "calendar.year", "ring.area"))),
           tag, ring.width)
ring.ducts_area_data<- spread((subset(ring_data, select=c("tag", "calendar.year", "resin.ducts_area"))),
                        tag, ring.width)