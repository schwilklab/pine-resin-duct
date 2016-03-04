# ringarea.R

# This code will read in tree ring widths calculated from rings.R and
# calculate the area between each ring and the preceeding ring.

# Variables needed: 

# w= width of tree core
# r1= radius of inner ring for calculation
# r2= radius of outer ring for calculation
# a1= chord area for r1
# a2= chord area for r2
# x.1 = r1 +/- sqrt(r^2-(w/2)^2)
# x.2 = r2 +/- sqrt(r^2-(w/2)^2))

# Final equation:

# ar= w * (r2-r1-x2+x1)

## I don't believe this equation is right.  I must have misunderstood 
## the variables and equation needed in order to perform this calculation.
## I would like to discuss this in our meeting today.  We can code this
## together, or if I can get the correct equation and values for each
## variable needed in the equation, I can attempt to do it myself.