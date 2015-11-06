# ringarea.R

# This code will read in tree ring widths calculated from rings.R and
# calculate the area between each ring and the preceeding ring.

# Variables needed: 

# w= width of tree core
# r1= inner ring for calculation
# r2= outer ring for calculation
# a1= chord area for r1
# a2= chord area for r2
# x1 = r1 +/- sqrt(r^2-(w/2)^2)
# x2 = r2 +/- sqrt(r^2-(w/2)^2))

# Final equation:

# ar= w * (r2-r1-x2-x1)

# NOTE: Need to create new data column in rings.R to reflect radius
# for each tree ring.