# Method: Given data sampled at interval dt, restrict tidem() to only consider
# constituents with period exceeding 0.5/dt.

library(oce)
data(sealevel)
data(tidedata)
t1 <- sealevel[["time"]]
e1 <- sealevel[["elevation"]]

# Interpolate to avoid issues with uneven sampling in t1
t6 <- seq(t1[1], tail(t1, 1), by="6 hour")
e6 <- approx(t1, e1, t6)$y

source("~/git/oce/R/tides.R")
m1 <- tidem(t1, e1)
m6 <- tidem(t6, e6)

# Method: Given data sampled at interval dt, restrict tidem() to only consider
# constituents with period exceeding 0.5/dt.
dt <- 6 # sampling rate
#keep <- tidedata$const$freq < (0.5 - sqrt(.Machine$double.eps)) / dt
keep <- tidedata$const$freq < 0.5 / dt
const <- tidedata$const$name[keep]
freq <- tidedata$const$freq[keep]
m6 <- tidem(t6, e6, constituents=const)
summary(m6)
par(mfcol=c(3, 2))
tlim <- t1[c(1, 28*24)] # 4 weeks

plot(m1, xlim=c(0, 0.35), cex=0.6, constituents=c("K1", "S2", "S4"), ylim=c(0, 2.4))
oce.plot.ts(t1, e1, xlim=tlim, drawTimeRange=FALSE)
oce.plot.ts(t1, predict(m1), xlim=tlim, drawTimeRange=FALSE)

plot(m6, xlim=c(0, 0.35), cex=0.6, constituents=c("K1", "S2", "S4"), ylim=c(0, 2.4))
oce.plot.ts(t6, e6, xlim=tlim, drawTimeRange=FALSE)
oce.plot.ts(t6, predict(m6), xlim=tlim, drawTimeRange=FALSE)
