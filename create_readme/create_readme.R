library(oce)
data(ctd)

## ../oce-demo-1.png: PNG image data, 1050 x 1050, 8-bit/color RGBA, non-interlaced
png("oce-demo-1.png", width=1050, height=1050, pointsize=26)
par(lwd=3)
plot(ctd, which=c(1,2,3,5), type="l", span=150)
dev.off()


## ../oce-demo-2.png: PNG image data, 1050 x 1050, 8-bit/color RGBA, non-interlaced
data(adp)
png("oce-demo-2.png", width=1050, height=1050, pointsize=32)
par(lwd=3)
plot(adp)
dev.off()

## ../oce-demo-3.png: PNG image data, 1050 x 1050, 8-bit colormap, non-interlaced
data(sealevel)
m <- tidem(sealevel)
png("oce-demo-3.png", width=1050, height=1050, pointsize=26)
par(lwd=3)
par(mfrow=c(2, 1))
plot(sealevel, which=1)
plot(m)
dev.off()

## ../oce-demo-4.png: PNG image data, 1050 x 600, 8-bit/color RGBA, non-interlaced
data(echosounder)
png("oce-demo-4.png", width=1050, height=600, pointsize=26)
par(lwd=3)
plot(echosounder, which=2, drawTimeRange=TRUE, drawBottom=TRUE)
dev.off()

## ../oce-demo-5.png: PNG image data, 1050 x 600, 8-bit/color RGBA, non-interlaced
data(endeavour, package="ocedata")
data(coastlineWorld, package="oce")
png("oce-demo-5.png", width=1050, height=600, pointsize=26)
par(lwd=3)
par(mar=rep(0.5, 4))
mapPlot(coastlineWorld, col='gray')
mapPoints(endeavour$longitude, endeavour$latitude, pch=20, col='red')
dev.off()

## ../oce-demo-6.png: PNG image data, 600 x 600, 8-bit/color RGBA, non-interlaced
data(landsat)
png("oce-demo-6.png", width=600, height=600, pointsize=18)
par(lwd=3)
plot(landsat)
dev.off()

