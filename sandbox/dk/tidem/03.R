# Trial handling of 6-hourly data using data(sealevel)

library(oce)
source("~/git/oce/R/tides.R")
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

par(mfcol=c(3, 3))

oce.plot.ts(t1, e1, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("Δt=1h")
par(mfrow=c(1, 2))
plot(m1, which=2)
plot(m1, xlim=c(0, 0.5), which=2)
abline(v=1/6, col="magenta")
abline(v=0.5/6, col="magenta", lty=2)
spectrum(S1, spans=c(7, 5), main="")
abline(v=1/6, col="magenta")
abline(v=0.5/6, col="magenta", lty=2)
title("Δt=1h")

oce.plot.ts(t6, e6, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("Δt=6h")
plot(m6)
abline(v=1/6, col="magenta")
abline(v=0.5/6, col="magenta", lty=2)
spectrum(S6, spans=c(7, 5), xlim=c(0, 0.5), main="")
abline(v=1/6, col="magenta")
abline(v=0.5/6, col="magenta", lty=2)
title("Δt=6h")

oce.plot.ts(t1, e61, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("Δt=1h (from 6h)")
plot(m61)
abline(v=1/6, col="magenta")
abline(v=0.5/6, col="magenta", lty=2)
spectrum(S61, spans=c(7, 5), xlim=c(0, 0.5), main="")
abline(v=1/6, col="magenta")
abline(v=0.5/6, col="magenta", lty=2)
title("Δt=1h (from 6h)")


