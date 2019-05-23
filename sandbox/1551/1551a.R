library(oce)
rms <- function(x) sqrt(mean(x^2))
data(topoWorld) # for checking for consistency, after updates

if (!file.exists("t3.nc")) {
    url3 <- "https://gis.ngdc.noaa.gov/mapviewer-support/wcs-proxy/wcs.groovy?filename=etopo1.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=netcdf&resx=0.500000&resy=0.500000&bbox=-179.5,-89.5,180,90"
    download.file(url3, "t3.nc")
}
if (!exists("t3"))
    t3 <- read.topo("t3.nc")

expect_equal(t3[["latitude"]], topoWorld[["latitude"]])
expect_equal(t3[["longitude"]], topoWorld[["longitude"]])

z <- topoWorld[["z"]]
z3 <- t3[["z"]]

if (!interactive()) png("1551a.png", width=6, height=2, unit="in", res=150, pointsize=9)
par(mfrow=c(1, 3))
lx <- 1:50
ly <- 1:50
zlim <- range(c(z[lx,ly],z3[lx,ly]))
imagep(z[lx,ly], zlim=zlim, zlab="z")
imagep(z3[lx,ly], zlim=zlim, zlab="z3")
imagep((z-z3)[lx,ly], zlim="symmetric", zlab="z-z3")
mtext(sprintf("rms diff: %.0fm (view) and %.0fm (globe)",
              rms((z-z3)[lx,ly]), rms((z-z3))), cex=2/3)
if (!interactive()) dev.off()

