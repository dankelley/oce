library(oce)
data(topoWorld) # for checking for consistency, after updates
topoWorldOld <- topoWorld

## sink("D")
## https://gis.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz?filename=etopo1.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=netcdf&resx=0.500000&resy=0.500000&bbox=-179.500000,-89.500000,180.000000,90.000000 
topoFile <- download.topo(west=-180, east=180, south=-90, north=90,
                          resolution=30, format="netcdf", destdir=".")

topoWorld <- read.topo(topoFile, debug=5)
## sink()
str(topoWorldOld@data,1)
expect_true(is.vector(topoWorld[["longitude"]]))
expect_true(is.vector(topoWorld[["latitude"]]))
cat(vectorShow(topoWorldOld[["longitude"]]))
cat(vectorShow(topoWorld[["longitude"]]))
cat(vectorShow(topoWorldOld[["latitude"]]))
cat(vectorShow(topoWorld[["latitude"]]))

#cat("topoWorldTest does NOT equal topoWorld:\n")
## retain the tests, so 'make' breaks and the developer notices
##expect_equal(topoWorld[["longitude"]], topoWorldTest[["longitude"]])
##expect_equal(topoWorld[["latitude"]], topoWorldTest[["latitude"]])
if (!interactive()) png("create_topoWorld.png", unit="in", res=100, width=7, height=3.5, pointsize=9)
par(mfrow=c(2, 2))
zlim <- range(c(topoWorldOld[["z"]], topoWorld[["z"]]))
plot(topoWorldOld, location="none", lwd.land=0.5, lwd.water=0.5)
mtext("old")
imagep(topoWorldOld, zlim=zlim)
mtext("old")
plot(topoWorld, location="none", lwd.land=0.5, lwd.water=0.5)
mtext("new")
imagep(topoWorld, zlim=zlim)
mtext("new")
if (!interactive()) dev.off()

summary(topoWorldOld)

summary(topoWorld)

save(topoWorld, file="topoWorld.rda")
if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    tools::resaveRdaFiles('topoWorld.rda', version=2)
} else {
    tools::resaveRdaFiles('topoWorld.rda')
}


