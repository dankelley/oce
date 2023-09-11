# vim:textwidth=140:expandtab:shiftwidth=4:softtabstop=4
# See also https://www.github.com/dankelley/oce/issues/2146 (which
# covers more time, and shows a deviation that I do not understand,
# and will keep in mind as I read more about tides).  If I tried
# the present criterion across all time, it would fail,
# because the deviation gets as large as 0.004.

library(oce)

rms <- function(x) sqrt(mean(x^2, na.rm=TRUE))
# predictions from NOAA are given to 0.001 m resolution
eps <- 0.001 # see above, re issue 2146

# Honolulu
# https://tidesandcurrents.noaa.gov/stationhome.html?id=1612340
h <- read.delim("harmonics.tsv.gz", sep="\t")
o <- read.csv("predictions.csv.gz")
o$time <- as.POSIXct(o$Date.Time, tz="UTC")
# https://tidesandcurrents.noaa.gov/stationhome.html?id=1612340
latitude <- 21+18.2/60
m <- as.tidem(tRef=mean(o$time), latitude=latitude,
    name=h$Name, amplitude=h$Amplitude, phase=h$Phase)
pp <- predict(m, o$time)
zoffset <- mean(pp - o$Prediction)
pp <- pp - zoffset
rms(o$Prediction-pp)
expect_true(rms(o$Prediction-pp) < eps)

if (FALSE) {
    par(mfrow=c(2, 1))
    oce.plot.ts(o$time, o$Prediction)
    lines(o$time, pp, col=2, lty=2)
    oce.plot.ts(o$time, o$Prediction - pp)
}
