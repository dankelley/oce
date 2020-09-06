library(oce)
data(coastlineWorldFine, package="ocedata")
data(topoWorld)

## sink("D")
## https://gis.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz?filename=etopo1.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=netcdf&resx=0.500000&resy=0.500000&bbox=-179.500000,-89.500000,180.000000,90.000000 
topoFile <- download.topo(west=-180, east=180, south=-90, north=90,
                          resolution=10, format="netcdf", destdir=".")

topoWorldFine <- read.topo(topoFile, debug=5)

if (!interactive()) png("create_topoWorldFine_%d.png", unit="in", res=100, width=7, height=3.5, pointsize=9)
imagep(topoWorld, xlim=c(-70, -40), ylim=c(30, 50), asp=1/cos(pi/180*40), col=oceColorsGebco, zlim=c(-5000,0))
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
imagep(topoWorldFine, xlim=c(-70, -40), ylim=c(30, 50), asp=1/cos(pi/180*40), col=oceColorsGebco, zlim=c(-5000,0))
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
if (!interactive()) dev.off()

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("topoWorld")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(topoWorldFine, file="topoWorldFine.rda", version=2)
    tools::resaveRdaFiles('topoWorldFine.rda', version=2)
} else {
    save(topoWorldFine, file="topoWorldFine.rda")
    tools::resaveRdaFiles('topoWorldFine.rda')
}


