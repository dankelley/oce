### R code from vignette source 'oce.Rnw'

###################################################
### code chunk number 1: oce.Rnw:104-105
###################################################
options(keep.source=TRUE, width=60, prompt=' ', continue=' ')


###################################################
### code chunk number 2: oce.Rnw:117-119 (eval = FALSE)
###################################################
## library(oce)
## plot(read.oce(filename))


###################################################
### code chunk number 3: ctdplot (eval = FALSE)
###################################################
## library(oce)
## data(ctd)
## plot(ctd)


###################################################
### code chunk number 4: ctdsummary (eval = FALSE)
###################################################
## summary(ctd)


###################################################
### code chunk number 5: ctdnames (eval = FALSE)
###################################################
## names(ctd@metadata)


###################################################
### code chunk number 6: ctdfig
###################################################
library(oce)
data(ctd)
plot(ctd)


###################################################
### code chunk number 7: ctdrawplot (eval = FALSE)
###################################################
## data(ctdRaw)
## plotScan(ctdRaw)


###################################################
### code chunk number 8: ctdrawfig
###################################################
data(ctdRaw)
plotScan(ctdRaw)


###################################################
### code chunk number 9: ctdscaneg (eval = FALSE)
###################################################
## plotScan(ctdTrim(ctdRaw, "range",
##                  parameters=list(item="scan", from=140, to=250)))
## plotScan(ctdTrim(ctdRaw, "range",
##                  parameters=list(item="scan", from=150, to=250)))


###################################################
### code chunk number 10: ctdtrimeg (eval = FALSE)
###################################################
## ctdTrimmed <- ctdTrim(ctdRaw)


###################################################
### code chunk number 11: ctddectrim (eval = FALSE)
###################################################
## plot(ctdDecimate(ctdTrim(read.ctd("stn123.cnv"))))


###################################################
### code chunk number 12: ctdfix1 (eval = FALSE)
###################################################
## x <- read.ctd("nnsa_00934_00001_ct1.csv", type="WOCE")
## x[["institute"]] <- "SIO" # better (using an accessor) but still bad


###################################################
### code chunk number 13: ctdfix2 (eval = FALSE)
###################################################
## x <- read.ctd("nnsa_00934_00001_ct1.csv", type="WOCE")
## x <- oceEdit(x, "institute", "SIO") # better way


###################################################
### code chunk number 14: ctdfix3 (eval = FALSE)
###################################################
## x <- read.ctd("nnsa_00934_00001_ct1.csv", type="WOCE")
## x <- oceEdit(x, "institute", "SIO", "human-parsed", "Dan Kelley")


###################################################
### code chunk number 15: arcticeg (eval = FALSE)
###################################################
## library(oce)
## # Source: http://cchdo.ucsd.edu/data_access?ExpoCode=58JH199410
## files <- system("ls *.csv", intern=TRUE)
## for (i in 1:length(files)) {
##     cat(files[i], "\n")
##     x <- read.ctd(files[i])
##     if (i == 1) {
##         plotTS(x, xlim=c(31, 35.5), ylim=c(-1, 10), type="l", col="red")
##     } else {
##         lines(x[["salinity"]], x[["temperature"]], col="red")
##     }
## }


###################################################
### code chunk number 16: rangeg (eval = FALSE)
###################################################
## print(range(x[["temperature"]]))
## print(range(x[["salinity"]]))


###################################################
### code chunk number 17: sectionplot (eval = FALSE)
###################################################
## data(section)
## plot(section, which=c(1, 2, 3, 99))


###################################################
### code chunk number 18: oce.Rnw:413-414
###################################################
jpeg("section.jpg", quality=75, width=600, height=700, pointsize=12)


###################################################
### code chunk number 19: oce.Rnw:416-420
###################################################
data(section)
GS <- subset(section, 102<=stationId&stationId<=124)
GSg <- sectionGrid(GS, p=seq(0, 1600, 25))
plot(GSg, which=c(1,99), map.xlim=c(-85,-(64+13/60)))


###################################################
### code chunk number 20: oce.Rnw:422-423
###################################################
dev.off()


###################################################
### code chunk number 21: oce.Rnw:463-464
###################################################
jpeg("topo.jpg", quality=70, width=800, height=400, pointsize=13)


###################################################
### code chunk number 22: oce.Rnw:466-469
###################################################
library(oce)
data(topoWorld)
plot(topoWorld, clatitude=30, clongitude=370, span=9000)


###################################################
### code chunk number 23: oce.Rnw:471-472
###################################################
dev.off()


###################################################
### code chunk number 24: oce.Rnw:490-491
###################################################
jpeg("sealevel.jpg", quality=70, width=800, height=600, pointsize=20)


###################################################
### code chunk number 25: oce.Rnw:493-497
###################################################
library(oce)
#sealevel <- read.oce("../../tests/h275a96.dat")
data(sealevelHalifax)
plot(sealevelHalifax)


###################################################
### code chunk number 26: oce.Rnw:499-500
###################################################
dev.off()


###################################################
### code chunk number 27: oce.Rnw:577-578
###################################################
png("tdr.png", width=600, height=300, pointsize=13)


###################################################
### code chunk number 28: oce.Rnw:580-583
###################################################
library(oce)
data(tdr)
plot(tdr, useSmoothScatter=TRUE)


###################################################
### code chunk number 29: oce.Rnw:585-586
###################################################
dev.off()


###################################################
### code chunk number 30: oce.Rnw:616-617
###################################################
png("adp.png", width=800, height=400, pointsize=18)


###################################################
### code chunk number 31: adpplot
###################################################
library(oce)
data(adp)
plot(adp, which=1, adorn=expression({lines(x[["time"]], x[["pressure"]])}))


###################################################
### code chunk number 32: oce.Rnw:624-625
###################################################
dev.off()


###################################################
### code chunk number 33: oce.Rnw:693-699
###################################################
library(oce)
swRho(34, 10, 100)
swTheta(34, 10, 100)
swRho(34, swTheta(34, 10, 100), 0)
swRho(34, swTheta(34, 10, 100, 200), 200)
plotTS(as.ctd(c(30,40),c(-2,20),rep(0,2)), grid=TRUE, col="white")


###################################################
### code chunk number 34: oce.Rnw:704-709
###################################################
library(oce)
data(ctd)
pycnocline <- ctdTrim(ctd, "range",
                      parameters=list(item="pressure", from=5, to=12))
plotProfile(pycnocline, which="density+N2")


###################################################
### code chunk number 35: oce.Rnw:713-717
###################################################
library(oce)
data(ctd)
pycnocline <- subset(ctd, 5<=pressure & pressure<=12)
plotProfile(pycnocline, which="density+N2")


###################################################
### code chunk number 36: oce.Rnw:723-727
###################################################
library(oce)
data(section)
ctd <- as.ctd(section[["salinity"]], section[["temperature"]], section[["pressure"]])
plotTS(ctd)


###################################################
### code chunk number 37: oce.Rnw:731-742
###################################################
library(oce)
data(section)
SS <- TT <- pp <- id <- NULL
n <- length(section@data$station)
for (stn in section@data$station) {
    SS <- c(SS, stn[["salinity"]])
    TT <- c(TT, stn[["temperature"]])
    pp <- c(pp, stn[["pressure"]])
}
ctd <- as.ctd(SS, TT, pp)
plotTS(ctd)


###################################################
### code chunk number 38: oce.Rnw:750-763
###################################################
library(oce)
GS <- subset(section, 102<=stationId&stationId<=124)
dh <- swDynamicHeight(GS)
par(mfrow=c(2,1))
plot(dh$distance, dh$height, type="b", xlab="", ylab="Dyn. Height [m]")
grid()
# 1e3 metres per kilometre
f <- coriolis(GS@data$station[[1]]@metadata$latitude)
g <- gravity(GS@data$station[[1]]@metadata$latitude)
v <- diff(dh$height)/diff(dh$distance) * g / f / 1e3
plot(dh$distance[-1], v, type="l", col="blue", xlab="Distance [km]", ylab="Velocity [m/s]")
grid()
abline(h=0)


###################################################
### code chunk number 39: oce.Rnw:771-777
###################################################
library(oce)
data(sealevelHalifax)
# Focus on 2003-Sep-28 to 29th, the time when Hurricane Juan caused flooding
plot(sealevelHalifax,which=1,xlim=as.POSIXct(c("2003-09-24","2003-10-05"), tz="UTC"))
abline(v=as.POSIXct("2003-09-29 04:00:00", tz="UTC"), col="red")
mtext("Hurricane\nJuan", at=as.POSIXct("2003-09-29 04:00:00", tz="UTC"), col="red")


###################################################
### code chunk number 40: oce.Rnw:783-785
###################################################
library(oce)
data(sealevelHalifax)


###################################################
### code chunk number 41: oce.Rnw:788-789
###################################################
elevation <- sealevelHalifax[["elevation"]]


###################################################
### code chunk number 42: oce.Rnw:793-796
###################################################
spectrum(elevation, spans=c(3,7))
abline(v=1/12.42)
mtext("M2",at=1/12.42,side=3)


###################################################
### code chunk number 43: oce.Rnw:814-815
###################################################
abline(v=as.POSIXct("2008-06-25 00:00:00"),col="red")


