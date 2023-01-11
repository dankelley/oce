library(oce)
f <- "~/Downloads/test.adr"
# source("~/git/oce/R/adv.sontek.R")
d <- read.adv.sontek.adr(f, debug=1)
summary(d) # note odd numbers (min/max for 16-bit)
t<-d[["time"]]
v<-d[["v"]]
if (!interactive()) png("adr01_ts.png", unit="in", width=7, height=7, res=200)
par(mfrow=c(3, 1))
look <- seq(1, 500)
oce.plot.ts(t[look], v[look, 1])
look <- 500 + look
oce.plot.ts(t[look], v[look, 1])
look <- 400000 + look
oce.plot.ts(t[look], v[look, 1])
if (!interactive()) dev.off()

if (!interactive()) png("adr01_hist_%d.png", unit="in", width=7, height=7, res=200)
par(mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
hist(d[["heading"]], breaks=360)
hist(d[["pitch"]], breaks=360)
hist(d[["roll"]], breaks=360)
hist(d[["pressure"]], breaks=360)
hist(d[["temperature"]], breaks=360)
if (!interactive()) dev.off()
