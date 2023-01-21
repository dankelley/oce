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
plot(m1, xlim=c(0, 0.5), cex=0.6, constituents=c("S2", "S4"), ylim=c(0, 1.5))
showSomeFrequencies()
spectrum(S1, spans=c(7, 5), main="", ylim=c(1e-4, 1e2))
showSomeFrequencies()

oce.plot.ts(t6, e6, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("dt=6h")
plot(m6, xlim=c(0, 0.5), cex=0.6, constituents=c("S2", "S4"))
showSomeFrequencies()
spectrum(S6, spans=c(7, 5), xlim=c(0, 0.5), main="", ylim=c(1e-4, 1e2))
showSomeFrequencies()
# I see a step at 0.2469. Closest constituent is
data(tidedata)
tidedata$const$name[which.min(abs(0.2469-tidedata$const$freq))]

oce.plot.ts(t1, e61, drawTimeRange=FALSE, lwd=0.3, ylim=c(0, 3))
title("dt=1h (from 6h)")
plot(m61, xlim=c(0, 0.5), cex=0.6, constituents=c("S2", "S4"), ylim=c(0, 1.5))
showSomeFrequencies()
spectrum(S61, spans=c(7, 5), xlim=c(0, 0.5), main="", ylim=c(1e-4, 1e2))
showSomeFrequencies()

dt <- 6
freq <- tidedata$const$freq
keep <- freq < 0.5/dt
freq <- freq[keep]
name <- tidedata$const$name[keep]
order <- order(freq)
freq <- freq[order]
name <- name[order]
mm <- tidem(t6, e6, constituents=name)
summary(mm)
#plot(mm, xlim=c(0, 0.5))

message("in the prev summary, why is freq unordered?")
message("BUG: summary not in order of freq (because of substitutions, I suppose)")
message("CHECK: anything funny in tidem()?  Why not order there? BUT could break tests")
message("CHECK: can also order in summary()")
