## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(oce)

## ----eval=FALSE----------------------------------------------------------
#  d <- read.oce(f)

## ----eval=FALSE----------------------------------------------------------
#  d <- read.adp(f)

## ----eval=FALSE----------------------------------------------------------
#  f <- "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
#  dall  <- read.oce(f)

## ----eval=FALSE----------------------------------------------------------
#  d100 <- read.oce(f, by=100)

## ----eval=FALSE----------------------------------------------------------
#  read.oce(f, from=as.POSIXct("2008-06-26", tz="UTC")`,
#              to=as.POSIXct("2008-06-27", tz="UTC"),
#              by="60:00"
#              latitude=47.88126, longitude=-69.73433)

## ----results="hide"------------------------------------------------------
data(adp)
summary(adp)

## ----eval=FALSE----------------------------------------------------------
#  beam <- read.oce(f)
#  xyx <- beamToXyz(beam)
#  enu <- xyzToEnu(xyz, declination=-18.1)

## ----fig.height=7--------------------------------------------------------
plot(adp)

## ----fig.height=7--------------------------------------------------------
plot(subset(adp, distance<20))

## ------------------------------------------------------------------------
time <- adp[["time"]]
distance <- adp[["distance"]]

## ------------------------------------------------------------------------
v <- adp[["v"]]

## ----results="hide"------------------------------------------------------
sort(names(adp[["metadata"]]))

## ------------------------------------------------------------------------
adp[["originalCoordinate"]]
adp[["oceCoordinate"]]

## ------------------------------------------------------------------------
processingLogShow(adp)

## ------------------------------------------------------------------------
plot(adp, which="uv")

## ----fig.height=7--------------------------------------------------------
plot(subset(adp, time < median(adp[["time"]])))

## ------------------------------------------------------------------------
time <- adp[["time"]]
v <- adp[["v"]]
# The second index is for bin number, the third for beam number
midIndex <- dim(v)[2]/2
eastMid <- v[, midIndex, 1] # third index is beam
distance <- adp[["distance"]][midIndex]
oce.plot.ts(time, eastMid, ylab="Eastward velocity [m/s]")
## Depth mean; note that na.rm, is passed by apply() to mean()
eastMean <- apply(v[,,1], 1, mean, na.rm=TRUE)
lines(time, eastMean, col=2)
legend("top", lwd=1, col=1:2,
       legend=c(paste("At", distance, "m"), "Depth Avg"))

## ------------------------------------------------------------------------
u <- adp[["v"]][,,1]
v <- adp[["v"]][,,2]
ok <- is.finite(u) & is.finite(v) # remove NA values
u <- u[ok]
v <- v[ok]
eigen(cov(data.frame(u, v)))

## ------------------------------------------------------------------------
pr <- prcomp(data.frame(u,v))

## ------------------------------------------------------------------------
pr

## ------------------------------------------------------------------------
time <- adp[["time"]]
pressure <- adp[["pressure"]]
oce.plot.ts(time, pressure)

## ------------------------------------------------------------------------
m <- tidem(as.sealevel(pressure, time))

## ------------------------------------------------------------------------
summary(m)

## ------------------------------------------------------------------------
oce.plot.ts(time, pressure, type="p", col="blue")
timePredict <- seq(min(time), max(time), length.out=200)
pressurePredict <- predict(m, timePredict)
lines(timePredict, pressurePredict, col="red")

