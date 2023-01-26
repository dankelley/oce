# Get drifter by ID

library(oce)
source("~/git/oce/R/tides.R")

#http://osmc.noaa.gov/erddap/tabledap/gdp_interpolated_drifter.csv?ID%2Clongitude%2Clatitude%2Ctime%2Ctemp&ID=%22300234063700990%22

ID <- "2300234063700990"
url <- paste0("http://osmc.noaa.gov/erddap/tabledap/gdp_interpolated_drifter.csv?ID%2Clongitude%2Clatitude%2Ctime%2Ctemp&ID=%2",
    ID, "%22")
if (!file.exists("drifter.csv")) {
    download.file(url, "drifter.csv")
}
d <- read.csv("dan.csv", skip=1, col.names=c("ID", "longitude", "latitude", "time", "temperature"))
d$t <- as.POSIXct(gsub("[TZ]", " ", d$time), tz="UTC")

# Speed
lonRef <- mean(d$longitude)
latRef <- mean(d$latitude)
xy <- geodXy(d$longitude, d$latitude, longitudeRef=lonRef, latitudeRef=latRef)
t <- d$t
dt <- diff(as.numeric(t))
dx <- diff(xy$x)
dy <- diff(xy$y)
u <- dx / dt
v <- dy / dt
u <- dx / dt
u <- c(u[1], u)

# 6-h sampling
if (!interactive()) png("02.png", width=7, height=7, unit="in", res=200, pointsize=9)
par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
m <- tidem(t, u, debug=0)
oce.plot.ts(t, u, type="l", lwd=0.8, xaxs="i")
lines(t, predict(m), col=2, lwd=0.8)
summary(m)

# 6-h sampling but interpolated to 1-h
tt <- seq(t[1], tail(t,1), by=3600)
uu <- approx(t, u, tt)$y
mm <- tidem(tt, uu)
oce.plot.ts(tt, uu, type="l", lwd=0.8, xaxs="i")
lines(tt, predict(mm), col=2, lwd=0.8)
summary(mm)
if (!interactive()) dev.off()

