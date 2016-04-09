## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----eval=FALSE----------------------------------------------------------
#  library(oce)
#  d <- read.oce("station1.cnv")
#  summary(d)
#  plot(d)

## ----fig.cap="**Figure 2.** An overview of a ctd dataset.", fig.width=6, fig.height=6, dpi=72, dev.args=list(pointsize=15), message=FALSE----
library(oce)
data(ctd)
summary(ctd)
plot(ctd)

## ----fig.cap="**Figure 3.** Scanwise plot of the `ctdRaw` sample data set.  Note the spike at the start, the equilibration phase before the downcast, and the spurious freshening signal near the start of the upcast.", fig.width=5, fig.height=3, dpi=72, dev.args=list(pointsize=12)----
data(ctdRaw)
plotScan(ctdRaw)

## ----eval=FALSE----------------------------------------------------------
#  plotScan(ctdTrim(ctdRaw, "range",
#                   parameters=list(item="scan", from=140, to=250)))
#  plotScan(ctdTrim(ctdRaw, "range",
#                   parameters=list(item="scan", from=150, to=250)))

## ----eval=FALSE----------------------------------------------------------
#  ctdTrimmed <- ctdTrim(ctdRaw)

## ----eval=FALSE----------------------------------------------------------
#  plot(ctdDecimate(ctdTrim(read.ctd("stn123.cnv"))))

## ----eval=FALSE----------------------------------------------------------
#  library(oce)
#  # http://cchdo.ucsd.edu/data/7971/ar18_58JH19941029_ct1.zip
#  # setwd("~/Downloads/ar18_58JH19941029_ct1")
#  files <- system("ls *.csv", intern=TRUE)
#  for (i in 1:length(files)) {
#      x <- read.ctd(files[i])
#      if (i == 1) {
#          plotTS(x, Slim=c(31, 35.5), Tlim=c(-2, 10), type='o')
#      } else {
#          points(x[["salinity"]], x[["potential temperature"]])
#          lines(x[["salinity"]], x[["potential temperature"]])
#      }
#  }

## ----eval=FALSE----------------------------------------------------------
#  data(section)
#  plot(section, which=c(1, 2, 3, 99))

## ----fig.cap="**Figure 4.** Portion of the CTD section designated A03, showing the Gulf Sream region.  The square on the cruise track corresponds to zero distance on the section.", fig.width=5, fig.height=5, dpi=72, dev.args=list(pointsize=12)----
library(oce)
data(section)
GS <- subset(section, 102 <= stationId & stationId <= 124)
GSg <- sectionGrid(GS, p=seq(0, 1600, 25))
plot(GSg, which=c(1,99), map.xlim=c(-85,-(64+13/60)))

## ----fig.cap="**Figure 5.** World in Winkel Tripel projection, with the `section` profile sites indicated.", fig.width=4, fig.height=2.6, dpi=72----
library(oce)
data(coastlineWorld)
par(mar=rep(0, 4))
mapPlot(coastlineWorld, projection="+proj=wintri", col="lightgray")
data(section)
lon <- section[["longitude", "byStation"]]
lat <- section[["latitude", "byStation"]]
mapLines(lon, lat, col='red', cex=0.5)

## ----echo=FALSE----------------------------------------------------------
data(coastlineWorld)
par(mar=rep(1, 4))

## ----fig.keep="none"-----------------------------------------------------
mapPlot(coastlineWorld, projection="+proj=moll") # Molleweide
mapPlot(coastlineWorld, projection="+proj=eck4") # Eckert IV
mapPlot(coastlineWorld, projection="+proj=robin") # Robinson

## ----fig.cap="**Figure 6.** World bathymetry in Molleweide projection.", fig.width=5, fig.height=2.7, dpi=72, dev.args=list(pointsize=10)----
par(mar=c(1.5, 1, 1.5, 1))
data(topoWorld)
topo <- decimate(topoWorld, 2) # coarsen grid: 4X faster plot
lon <- topo[["longitude"]]
lat <- topo[["latitude"]]
z <- topo[["z"]]
cm <- colormap(name="gmt_globe")
drawPalette(colormap=cm)
mapPlot(coastlineWorld, projection="+proj=moll", grid=FALSE)
mapImage(lon, lat, z, colormap=cm)

## ----fig.cap="**Figure 7.** North Atlantic in Lambert Conformal Conic projection.", fig.width=5, fig.height=3, dpi=72----
par(mar=c(2, 2, 1, 1))
lonlim <- c(-80, 0)
latlim <- c(20, 60)
mapPlot(coastlineWorld, projection="+proj=lcc +lat_1=30 +lat_2=50 +lon_0=-40",
        longitudelim=lonlim, latitudelim=latlim)

## ---- fig.keep="none"----------------------------------------------------
mapPlot(coastlineWorld, projection="+proj=merc",
        longitudelim=lonlim, latitudelim=latlim)
mapPlot(coastlineWorld, projection="+proj=aea +lat_1=30 +lat_2=70 +lon_0=-40",
        longitudelim=lonlim, latitudelim=latlim)

## ----fig.cap="**Figure 8.** Arctic coastlines in stereopolar projection.", fig.width=5, fig.height=5, dpi=72----
par(mar=c(2, 2, 1, 1))
mapPlot(coastlineWorld, projection="+proj=stere +lat_0=90",
        longitudelim=c(-80,0), latitudelim=c(70, 110))

## ----fig.cap="**Figure 9.** Sea-level timeseries measured in 2003 in Halifax Harbour.", fig.width=7, fig.height=5, dpi=72, dev.args=list(pointsize=16)----
library(oce)
data(sealevel)
plot(sealevel)

## ----fig.cap="**Figure 10.** Measurements made with a bottom-mounted ADP in the St Lawrence Estuary. The line near the surface indicates pressure measured by the ADP.", fig.width=5, fig.height=2, dpi=72----
library(oce)
data(adp)
plot(adp, which=1)
lines(adp[['time']], adp[['pressure']], lwd=2)

## ------------------------------------------------------------------------
data(section)
stn <- section[["station", 100]]
stn[["salinityFlag"]]

## ------------------------------------------------------------------------
# fake second datum
stn[["salinity"]][2] <- -999

## ------------------------------------------------------------------------
stn[["salinityFlag"]]

## ------------------------------------------------------------------------
stn2 <- stn
stn2[["salinity"]] <- ifelse(stn[["salinityFlag"]]!=2, NA, stn[["salinity"]])

## ------------------------------------------------------------------------
stn3 <- handleFlags(stn, list(salinity=3))

## ------------------------------------------------------------------------
stn3 <- handleFlags(stn, flags=list(salinity=3), actions=list(salinity="NA"))

## ------------------------------------------------------------------------
head(stn[["salinity"]]) # no NA values
head(stn2[["salinity"]]) # NA at positions 2 and 6
head(stn3[["salinity"]]) # as previous

## ----fig.keep="none"-----------------------------------------------------
library(oce)
Sys.setenv(LANGUAGE="fr")
data(ctd)
plot(ctd)

## ----echo=FALSE----------------------------------------------------------
Sys.setenv(LANGUAGE="en")

## ----fig.width=5, fig.height=5, fig.keep="none"--------------------------
library(oce)
swRho(34, 10, 100)
swTheta(34, 10, 100)
swRho(34, swTheta(34, 10, 100), 0)
swRho(34, swTheta(34, 10, 100, 200), 200)
plotTS(as.ctd(c(30,40),c(-2,20),rep(0,2)), grid=TRUE, col="white")

## ----fig.width=5, fig.height=5, fig.keep="none"--------------------------
library(oce)
data(ctd)
pycnocline <- ctdTrim(ctd, "range",
                      parameters=list(item="pressure", from=5, to=12))
plotProfile(pycnocline, which="density+N2")

## ----fig.width=5, fig.height=5, fig.keep="none"--------------------------
library(oce)
data(ctd)
pycnocline <- subset(ctd, 5 <= pressure & pressure <= 12)
plotProfile(pycnocline, which="density+N2")

## ----eval=FALSE----------------------------------------------------------
#  library(oce)
#  # http://cchdo.ucsd.edu/data/7971/ar18_58JH19941029_ct1.zip
#  # setwd("~/Downloads/ar18_58JH19941029_ct1")
#  files <- system("ls *.csv", intern=TRUE)
#  n <- length(files)
#  ctds <- vector("list", n) # to hold the CTD objects
#  station <- vector("list", n)
#  for (i in 1:n) {
#      ctds[[i]] <- read.ctd(files[i])
#      station[[i]] <- ctds[[i]][["station"]]
#  }
#  S <- unlist(lapply(1:n, function(i) ctds[[i]][["salinity"]]))
#  T <- unlist(lapply(1:n, function(i) ctds[[i]][["temperature"]]))
#  p <- unlist(lapply(1:n, function(i) ctds[[i]][["pressure"]]))
#  overall <- as.ctd(S, T, p)
#  png("ar18_%02d.png")
#  for (i in 1:n) {
#      plotTS(overall, col='gray')
#      lines(ctds[[i]][["salinity"]], ctds[[i]][["potential temperature"]])
#      mtext(station[i], side=3, line=0)
#  }
#  dev.off()

## ----fig.width=5, fig.height=5, fig.keep="none"--------------------------
library(oce)
data(section)
ctd <- as.ctd(section[["salinity"]], section[["temperature"]], section[["pressure"]])
col <- ifelse(section[["longitude"]] > -30, "black", "gray")
plotTS(ctd, col=col)

## ----fig.width=5, fig.height=3, fig.keep="none"--------------------------
library(oce)
data(section)
GS <- subset(section, 102 <= stationId & stationId <= 124)
dh <- swDynamicHeight(GS)
par(mfrow=c(2,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
plot(dh$distance, dh$height, type="l", xlab="", ylab="Dyn. Height [m]")
grid()
# 1e3 metres per kilometre
latMean <- mean(GS[["latitude"]])
f <- coriolis(latMean)
g <- gravity(latMean)
v <- diff(dh$height)/diff(dh$distance) * g / f / 1e3
plot(dh$distance[-1], v, type="l", xlab="Distance [km]", ylab="Velocity [m/s]")
grid()
abline(h=0, col='red')

## ----fig.width=7, fig.height=3, fig.keep="none"--------------------------
library(oce)
data(sealevel)
# Focus on 2003-Sep-28 to 29th, the time when Hurricane Juan caused flooding
plot(sealevel,which=1,xlim=as.POSIXct(c("2003-09-24","2003-10-05"), tz="UTC"))
abline(v=as.POSIXct("2003-09-29 04:00:00", tz="UTC"), col="red")
mtext("Juan", at=as.POSIXct("2003-09-29 04:00:00", tz="UTC"), col="red")

## ----results="hide", fig.keep="none"-------------------------------------
library(oce)
data(sealevel)
m <- tidem(sealevel)
oce.plot.ts(sealevel[['time']], sealevel[['elevation']] - predict(m),
            ylab="Detided sealevel [m]", 
            xlim=c(as.POSIXct("2003-09-20"), as.POSIXct("2003-10-08")))

