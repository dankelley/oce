library(oce)
f <- "~/Dropbox/data/archive/sleiwex/2008/moorings/m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr"
d <- read.adv.sontek.adr(f, debug=1)
summary(d)
time <-d[["time"]]
pressure <-d[["pressure"]]
temperature <-d[["temperature"]]
v <- d[["v"]]
n <- length(time)
if (!interactive()) png("adr02_ts.png", unit="in", width=7, height=7, res=200)
par(mfrow=c(3, 1))
look <- n/2 + seq(1, 1000)
oce.plot.ts(time[look], v[look, 1])
oce.plot.ts(time[look], pressure[look])
oce.plot.ts(time[look], temperature[look])
if (!interactive()) dev.off()

if (!interactive()) png("adr02_hist_%d.png", unit="in", width=7, height=7, res=200)
par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
hist(d[["heading"]], breaks=360)
hist(d[["pitch"]], breaks=360)
hist(d[["roll"]], breaks=360)
hist(d[["pressure"]], breaks=360)
hist(d[["temperature"]], breaks=360)
if (!interactive()) dev.off()
