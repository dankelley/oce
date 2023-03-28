
library(oce)
data(sealevel)
t1 <- sealevel[["time"]]
e1 <- sealevel[["elevation"]]
S1 <- ts(e1, deltat=1)

t6 <- seq(t1[1], tail(t1, 1), by="6 hour")
e6 <- approx(t1, e1, t6)$y
S6 <- ts(e6, deltat=6)

e61 <- approx(t6, e6, t1)$y
e61[is.na(e61)] <- mean(e61, na.rm=TRUE)
S61 <- ts(e61, deltat=1)


m1 <- tidem(t1, e1)
m6 <- tidem(t6, e6)
m61 <- tidem(t1, e61)

showSomeFrequencies <- function()
{
    abline(v=1/6, col="red")
    abline(v=0.5/6, col="green")
}

par(mfcol=c(3, 3))

oce.plot.ts(t1, e1, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("dt=1h")
plot(m1, xlim=c(0, 0.35), cex=0.6, constituents=c("S2", "S4"), ylim=c(0, 1.4))
showSomeFrequencies()
spectrum(S1, spans=c(7, 5), main="")
showSomeFrequencies()

oce.plot.ts(t6, e6, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("dt=6h")
plot(m6, xlim=c(0, 0.35), cex=0.6, constituents=c("S2", "S4"))
showSomeFrequencies()
spectrum(S6, spans=c(7, 5), xlim=c(0, 0.5), main="")
showSomeFrequencies()
# I see a step at 0.2469. Closest constituent is
data(tidedata)
tidedata$const$name[which.min(abs(0.2469-tidedata$const$freq))]

oce.plot.ts(t1, e61, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("dt=1h (from 6h)")
plot(m61, xlim=c(0, 0.35), cex=0.6, constituents=c("S2", "S4"), ylim=c(0, 1.4))
showSomeFrequencies()
spectrum(S61, spans=c(7, 5), xlim=c(0, 0.5), main="")
showSomeFrequencies()

# Method: Given data sampled at interval dt, restrict tidem() to only consider
# constituents with period exceeding 0.5/dt.
dt <- 6 # sampling rate
keep <- tidedata$const$freq < (0.5 - sqrt(.Machine$double.eps)) / dt
keep <- tidedata$const$freq < (0.5 - sqrt(.Machine$double.eps)) / dt
const <- tidedata$const$name[keep]
freq <- tidedata$const$freq[keep]
mm <- tidem(t6, e6, constituents=const)
summary(mm)
par(mfrow=c(2, 1))
plot(m1, xlim=c(0, 0.35), cex=0.6, constituents=c("S2", "S4"), ylim=c(0, 2.4))
plot(mm, xlim=c(0, 0.35), cex=0.6, constituents=c("S2", "S4"), ylim=c(0, 2.4))
par(mfrow=c(2, 1))
tlim <- t1[c(1, 14*24)] # two weeks
oce.plot.ts(t1, e1, xlim=tlim, drawTimeRange=FALSE)
points(t6, e6, pch=20)
lines(t1, predict(m1), col=2)
oce.plot.ts(t6, e6, xlim=tlim, drawTimeRange=FALSE)
lines(t6, predict(mm), col=2)

#source("~/git/oce/R/tides.R");plot(mm)#, xlim=c(0, 0.5))

