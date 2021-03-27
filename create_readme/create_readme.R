library(oce)
data(ctd)

f <- 0.8 # trim files, in hopes of making package under 5e6 bytes

## ../oce-demo-1.png: PNG image data, 1050 x 1050, 8-bit/color RGBA, non-interlaced
png("oce-demo-1.png", width=f*1050, height=f*1050, pointsize=26)
par(lwd=3)
plot(ctd, which=c(1,2,3,5), type="l", span=150)
dev.off()


## ../oce-demo-2.png: PNG image data, 1050 x 1050, 8-bit/color RGBA, non-interlaced
data(adp)
png("oce-demo-2.png", width=f*1050, height=f*1050, pointsize=32)
par(lwd=3)
plot(adp)
dev.off()

## ../oce-demo-3.png: PNG image data, 1050 x 1050, 8-bit colormap, non-interlaced
data(sealevel)
m <- tidem(sealevel)
png("oce-demo-3.png", width=f*1050, height=f*1050, pointsize=26)
par(lwd=3)
par(mfrow=c(2, 1))
plot(sealevel, which=1)
plot(m)
dev.off()

## ../oce-demo-4.png: PNG image data, 1050 x 600, 8-bit/color RGBA, non-interlaced
data(echosounder)
png("oce-demo-4.png", width=f*1050, height=f*600, pointsize=26)
par(lwd=3)
plot(echosounder, which=2, drawTimeRange=TRUE, drawBottom=TRUE)
dev.off()

## ../oce-demo-5.png: PNG image data, 1050 x 600, 8-bit/color RGBA, non-interlaced
data(endeavour, package="ocedata")
data(coastlineWorld, package="oce")
png("oce-demo-5.png", width=f*1050, height=f*600, pointsize=26)
par(lwd=3)
par(mar=rep(0.5, 4))
mapPlot(coastlineWorld, col='gray')
mapPoints(endeavour$longitude, endeavour$latitude, pch=20, col='red')
dev.off()

## ../oce-demo-6.png: PNG image data, 600 x 600, 8-bit/color RGBA, non-interlaced
data(landsat)
png("oce-demo-6.png", width=f*600, height=f*600, pointsize=18)
par(lwd=3)
plot(landsat)
dev.off()

# NOTES on trimming size for 1.4-0 CRAN submission.
#
# The 1.4-0 submitted tarball was 5.155e6, which exceeds CRAN 'pretest' limit
# of 5.00e6.
#
#  Using f=0.8 keeps figures nice and clean, but trims as follows, which, at
#  0.208e6, will get us (just) under the limit.
newSize <- 126188+101694+166633+80698+133887+42474
oldSize <- 167435+124723+237887+95196+185154+49166
cat("Trimming by using f=", f, "is", oldSize-newSize, "bytes\n")


