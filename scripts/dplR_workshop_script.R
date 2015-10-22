##########################################################################
#
# 0 Intro
#
##########################################################################

# Hello. This is the code I used for the dplR workshop at AmeriDendro
# in May 2013 in Tucson. It depends on having a few R packages that you
# might or might not have installed. If you don't have dplR, forecast,
# bootRes or other packages installed you can do so via the install.package()
# function e.g., 
# install.package('dplR')
# You should also update all your packages regularly - it's a good thing to
# do regularly:
update.packages()

# start with a clean workspace
rm(list=ls())
# load dplR
library(dplR)
# save default graphing parameters to reset
# This step is useful because R has a lot
# of graphing parameters that we will change
# for some plots and then want to reset before
# making the next plot
op <- par(no.readonly=TRUE)

##########################################################################
#
# 1 Basics
#
##########################################################################

# Note that a lot of these basic functions are decribed in:
# Bunn, A.G. 2008. A dendrochronology program library in R (dplR). 
# Dendrochronologia, 26: 115-124.
# A PDF is available on my ResearchGate profile:
# http://www.researchgate.net/profile/Andrew_Bunn/

# set the working directory, note / vs \\ for Windows
setwd('/Users/lindbeem/Desktop/workshop_dendro/')

##########################################################################
#
# 1.1 Reading data
#
##########################################################################

# In the workshop I used wa084. You can find it on the ITRDB.
# I'm going to assume that you are loading wa084 rather than using
# another dataset

# read in the Mt. Angeles ring width file
#wa084 <- read.rwl('wa084.rwl')

# Data needs to be downloaded from website.  Will use ca533 data. It's
# already loaded into R.
data(ca533)
# plot segments
spag.plot(ca533)
ca533.stats <- rwl.stats(ca533)
# stats on each series
ca533.stats

##########################################################################
#
# 1.2 Detrending
#
##########################################################################

# Let's detrend all the series in ca533 with a spline:
ca533.rwi <- detrend(ca533, method="Spline")

# interactively detrend wa084 - but detrendeR is better! Use: library(detrendeR)
# if you must use dplR:
# wa084.rwi <- i.detrend(wa084)

# Some descriptive stats on each series
ca533.ids <- read.ids(ca533.rwi, stc = 'auto')
ca533.rwi.stats <- rwi.stats(ca533.rwi,ids=ca533.ids)
# look at the series ids and the stats
ca533.ids
ca533.rwi.stats

##########################################################################
#
# 1.3 Chronology building
#
##########################################################################

# Build a chronology using the defaults (e.g., Tukey's biweight robust mean)
wa084.crn <- chron(wa084.rwi, prefix="MTA")
# Plot the chronology
crn.plot(wa084.crn,add.spline=T)
# Take a peak at the end of the chronology object
tail(wa084.crn)

##########################################################################
#
# 1.3.1 Truncate the chronology to have sample depth > 5
#
##########################################################################

wa084.trunc <- subset(wa084.crn, samp.depth > 5)
# and plot
crn.plot(wa084.trunc,add.spline=T)

# Many dplR functions are just little shortcuts. 
# E.g., let's make a prettier chronology plot
yrs <- as.numeric(row.names(wa084.trunc))
MTAstd <- wa084.trunc[,1] #wa084.trunc$MTAstd
par(tcl = 0.5, mgp = c(1.25, 0.25, 0),xaxs="i", mar=rep(3,4))
plot(yrs, MTAstd, type = "n", ylab = "Ring Width Index", xlab="Year",
     axes=F)
grid(lty=1,col="grey")
lines(yrs,MTAstd,col="grey10")
spl.16 <- ffcsaps(MTAstd, nyrs = 16)
lines(yrs,spl.16, col = "red", lwd = 2)
spl.64 <- ffcsaps(MTAstd, nyrs = 64)
lines(yrs,spl.64, col = "green", lwd = 2)
## nyrs defaults to 0.5*length(MTAstd) == 111
spl.def <- ffcsaps(MTAstd)
lines(yrs,spl.def, col = "blue", lwd = 2)
legend("bottomright",
       c("Chronology", "nyrs=16", "nyrs=64",
         paste("Default nyrs (", floor(length(MTAstd) / 2), ")", sep="")),
       fill=c("grey10", "red", "green", "blue"),bg="grey90")
axis(1);axis(2);axis(3);axis(4)
box()
par(op)

##########################################################################
#
# 1.3.2 Truncate the chronology via eps cutoff - more complicated
#
##########################################################################

foo <- rwi.stats.running(wa084.rwi, wa084.ids, window.length = 32)

yrs <- as.numeric(rownames(wa084.crn))
bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)),
                  eps = c(NA, foo$eps, NA))
par(mar = c(3, 3, 3, 3), mgp = c(1.25, 0.25, 0), tcl = 0.25)
plot(yrs, wa084.crn[, 1], type = "n", xlab = "Years", ylab = "RWI")
xx <- c(500, 500, max(bar$yrs[bar$eps < 0.8], na.rm = TRUE),
        max(bar$yrs[bar$eps < 0.8], na.rm = TRUE))
yy <- c(-1, 3, 3, -1)
polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
lines(yrs, wa084.crn[, 1], col = "grey50")
lines(yrs, ffcsaps(wa084.crn[, 1], nyrs = 32), col = "red", lwd = 2)
par(new = TRUE)
# Second plot is eps
plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "", axes = FALSE,
     pch = 20, col = "blue")
axis(4, at = pretty(foo$eps))
mtext("EPS", side = 4, line = 1.25)
box()
par(op)

##########################################################################
#
# 1.3.3 Diversion alert! Look at strip.rwl and then making your own 
# detrending function.
#
##########################################################################
?strip.rwl
data(anos1)
anos1.ids <- read.ids(anos1, stc = c(4, 3, 1))
anos1.strip <- strip.rwl(anos1, ids = anos1.ids, verbose = TRUE)
dim(anos1)
dim(anos1.strip)
# strip.rwl does double detrending
anos1.strip.rwi <- detrend(detrend(anos1.strip,method="Spline",nyrs=20),method="Spline",nyrs=200)

# compare to simple spline:
anos1.rwi <- detrend(detrend(anos1,method="Spline",nyrs=200),method="Spline",nyrs=20)
rwi.stats(anos1.rwi,anos1.ids)
rwi.stats(anos1.strip.rwi,anos1.ids)
plot(anos1.strip.rwi[,3],anos1.rwi[,3])

# more fun. show how double detrending is very similar
# to taking the residuals from an ar model. this also
# shows that creating your own methods for doing something
# like detrending is easy
detrend.ar <- function(x) {
  y <- x
  idx.goody <- !is.na(y)
  ar1 <- ar(y[idx.goody])
  y2 <- ar1$resid+ar1$x.mean
  y[idx.goody] <-  y2
  y
}

anos1.ar.rwi <- data.frame(apply(detrend(anos1,method='Mean'),2,detrend.ar))
rwi.stats(anos1.rwi,anos1.ids)
rwi.stats(anos1.ar.rwi,anos1.ids)
plot(anos1.ar.rwi[,1],anos1.rwi[,1])

##########################################################################
#
# 2 Time series stuff
#
##########################################################################
yrs <- as.numeric(row.names(wa084.trunc))
MTAstd <- wa084.trunc[,1] #wa084.trunc$MTAstd

# dplR has some prepackaged functions (mostly ones the developers use)

nYrs <- length(MTAstd)
nPwrs2 <- trunc(log(nYrs)/log(2)) - 1
out.wave <- morlet(y1 = MTAstd, x1 = yrs, p2 = nPwrs2, dj = 0.1,
                   siglvl = 0.99)
wavelet.plot(out.wave)

# But...
# The power of working with dplR has little to do with the dplR functions
# themselves but rather the libraries that are available. E.g.,:

# Do a MRA on the chron
library(waveslim)

MTAstd.mra <- mra(MTAstd, wf = "la8", J = nPwrs2, method = "modwt",
                  boundary = "periodic")
YrsLabels <- paste(2^(1:nPwrs2),"yrs",sep="")

par(mar=c(3,2,2,2),mgp=c(1.25,0.25,0),tcl=0.25,tck=0.0125)
plot(yrs,rep(1,nYrs),type="n", axes=FALSE, ylab="",xlab="",
     ylim=c(-2,27))
title(main="Multiresolution decomposition of MTAstd",line=0.75)
axis(side=1)
mtext("Years",side=1,line = 1.25)
Offset <- 0
for(i in nPwrs2:1){
  x <- scale(MTAstd.mra[[i]]) + Offset
  lines(yrs,x)
  abline(h=Offset,lty="dashed")
  mtext(names(MTAstd.mra)[[i]],side=2,at=Offset,line = 0)
  mtext(YrsLabels[i],side=4,at=Offset,line = 0)
  Offset <- Offset+5
}
box()
par(op) #reset par

# or interogating a time series
acf(MTAstd,ci = 0.99)
pacf(MTAstd,ci = 0.99)
MTA.ar <- ar(MTAstd,order.max=5)
MTA.ar
str(MTA.ar)
acf(MTA.ar$resid,na.action=na.pass)
plot(yrs,MTA.ar$resid,type='l')

# ar fits an ar(p) model by aic
# want to do an arima in the same way?
library(forecast)
MTA.arima <- auto.arima(MTAstd)
# so it looks like our ar model was good!
MTA.arima
# compare and look at an acf of the residuals
MTA.ar
acf(MTA.arima$resid,na.action=na.pass,ci=0.99)

##########################################################################
#
# 3 Cross dating
#
##########################################################################

# Note that these functions are decribed in:
# Bunn, A.G. 2010. Statistical and visual crossdating in R using the dplR library. 
# Dendrochronologia. Dendrochronologia 28: 251-258.
# A PDF is available on my ResearchGate profile:
# http://www.researchgate.net/profile/Andrew_Bunn/

# These data are nicely xdated. Let's break it!
dat <- wa084
dat$'693082' <- NULL
tmp <- dat$'693061'
tmp <- c(NA,tmp[-126])
dat$'693061' <- tmp

# look at correlation for each series to the master
# in fifty year intervals
rwl.50 <- corr.rwl.seg(dat,seg.length=50)

# Take the misdated series and remove it from the rwl object
flagged <- dat$'693061'
names(flagged) <- rownames(dat)
dat$'693061' <- NULL

# Let's look at the misdated series carefully
seg.50 <- corr.series.seg(rwl=dat, series=flagged, seg.length=50)

# Look at a cross-correlation function
ccf.50 <- ccf.series.rwl(rwl=dat,series=flagged,seg.length=50,bin.floor=50)
# look at a dangerously short segment length - get a better idea of where that
# bad series is
ccf.20 <- ccf.series.rwl(rwl=dat,series=flagged,seg.length=20,bin.floor=50)

# that's too hectic. Zoom in.
win <- 1840:1900
dat.yrs <- as.numeric(rownames(dat))
dat.trunc <- dat[dat.yrs%in%win,]
flagged.yrs <- as.numeric(names(flagged))
flagged.trunc <- flagged[flagged.yrs%in%win]
names(flagged.trunc) <- rownames(dat.trunc)

ccf.20 <- ccf.series.rwl(rwl=dat.trunc,series=flagged.trunc,seg.length=20,bin.floor=10)
ccf.20 <- ccf.series.rwl(rwl=dat.trunc,series=flagged.trunc,seg.length=20,bin.floor=NULL)

# One more look - this time with a skeleton plot
dat.trunc=dat[dat.yrs%in%win,]
dat.yrs.trunc=dat.yrs[dat.yrs%in%win]
flagged.trunc=flagged[flagged.yrs%in%win]
flagged.yrs.trunc=flagged.yrs[flagged.yrs%in%win]
flagged.name="641143"

skel.plot(rw.vec=flagged.trunc,yr.vec=flagged.yrs.trunc,sname=flagged.name,master=FALSE)
skel.plot(rw.vec=rowMeans(dat.trunc,na.rm=T),yr.vec=dat.yrs.trunc,sname='MTAstd',master=TRUE)

# here is the year we removed. The xdating functions got us very close
rownames(wa084)[126]

##########################################################################
#
# 4 Other Stuff
#
##########################################################################


##########################################################################
#
# 4.1 Simulation
#
##########################################################################

# This is the exact script I ran to generate figure one of a paper I have in
# press about mean sensitivity (see poster at AmeriDendro)
# Bunn, A.G., Jansma, E., Korpela, M., and R.D. Westfall. 2013. 
# Using simulations and data to evaluate mean sensitivity as a 
# useful statistic in Dendrochronology. Dendrochronologia. In press.

nyrs <- 500
# Instead of writing to the default device let's write to a png device
# could do jpg, pdf, tiff, eps, etc.
png('Fig1.png',width=5,height=5,units='in',pointsize=8,bg='white',res=300)

par(mfrow=c(2,1),mar=c(0,2.5,2.5,2.5),mgp=c(1.25,0.25,0),tcl=0.5,xaxs='i',yaxs='i')
my.ylim <- c(-0.1,2.1)

# ar
phi <- 0.4
sigma <- 0.2
sigma0 <-(1 - phi^2)*sigma^2
sigma0 <- sqrt(sigma0)

x.ar <- arima.sim(list(ar=phi),n = nyrs, sd = sigma0)+1
x.ar[x.ar<=0] <- 0.001

# model is
#X[t] = phi*X[t-1] + e[t]
#where e is N(0,sigma)
zeta <- round(sens2(x.ar),2)

yrs <- 1:nyrs
plot(yrs,x.ar,type='n',ylim=my.ylim,xlab='',ylab='',axes=F)
grid(lty='solid',lwd=1.5)
abline(h=1,lwd=1.5)
lines(yrs,x.ar,col='grey20')
txt <- bquote(paste(phi==.(phi),', ', sigma==.(sigma),
                    ', ',zeta==.(zeta)))
text(10,0.05,txt,pos=4)
text(5,1.9,"A",pos=4,cex=2)

axis(1,labels=F);axis(2);axis(3);axis(4,labels=F);
box()

# ar - high
phi <- 0.8
sigma <- 0.4
sigma0 <-(1 - phi^2)*sigma^2
sigma0 <- sqrt(sigma0)

x.ar <- arima.sim(list(ar=phi),n = nyrs, sd = sigma0)+1
x.ar[x.ar<=0] <- 0.001

zeta <- round(sens2(x.ar),2)

par(mar=c(2.5,2.5,0,2.5),mgp=c(1.25,0.25,0),tcl=0.5,xaxs='i',yaxs='i')
plot(yrs,x.ar,type='n',ylim=my.ylim,xlab='Year',ylab='',axes=F)
grid(lty='solid',lwd=1.5)
abline(h=1,lwd=1.5)
lines(yrs,x.ar,col='grey20')
txt <- bquote(paste(phi==.(phi),', ', sigma==.(sigma),
                    ', ',zeta==.(zeta)))
text(10,0.05,txt,pos=4)
text(5,1.9,"B",pos=4,cex=2)
axis(1);axis(2,labels=F);axis(4);axis(3,labels=F)
box()

mtext('Ring-Width Index',side=2,line=1.5,at=2)

dev.off()
par(op)

##########################################################################
#
# 4.3 bootRes()
#
##########################################################################

# This is a seperate R library by Christian Zang that implements Biondi's
# DENDROCLIM response function program

library(bootRes)
data(muc.clim)
data(muc.spruce)
head(muc.clim)
head(muc.spruce)

# calculate and plot bootstrapped correlation function
dc <- dcc(muc.spruce, muc.clim, method = "corr",start=4,end=9)
# calculate and plot moving correlation function
dc.mov1 <- mdcc(muc.spruce, muc.clim, method = "corr",start=4,end=9)

#save to a pdf to demo another type of device
pdf(file='bootResExamples.pdf',width=10,height=7,pointsize=12)
dcplot(dc)
mdcplot(dc.mov1)
dev.off()

