## Read world topo dataset at 1/12 deg res, then subsample to 1/12 deg.
## File format described at
## http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/ETOPO5.txt
library(oce)
f <- file("etopo5.dat", "rb")
seek(f, 0, "end", rw="read")
fileSize <- seek(f, 0, origin="start", rw="read")
nlon <- 360*12
nlat <- 180*12
n <- fileSize / 2
expect_equal(n, nlon*nlat)
z <- readBin(f, integer(), size=2, endian="big", n=n)
close(f)

z <- t(matrix(z, nrow=nlat, byrow=TRUE)) # the nlat vs nlon is confusing
expect_equal(nlon, dim(z)[1])
expect_equal(nlat, dim(z)[2])
latitude <- seq(90, by=-1/12, length.out=nlat)
longitude <- seq(0, by=1/12, length.out=nlon)
zOrig <- z
longitudeOrig <- longitude
latitudeOrig <- latitude

## Subsample to 1/2 deg, i.e. every 6th point.
ilon <- seq.int(1, nlon, 6)
ilat <- seq.int(1, nlat, 6)
longitude <- longitude[ilon]
latitude <- latitude[ilat]
z <- z[ilon, ilat]

## Flip by latitude (do last to get even lat values)
nlat <- length(ilat)
nlon <- length(ilon)
latitude <- latitude[seq.int(nlat,1)]
z <- z[, seq.int(nlat, 1)]

topoWorld <- as.topo(longitude, latitude, z, filename="etopo5.dat")

save(topoWorld, file="topoWorld.rda")
if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    tools::resaveRdaFiles('topoWorld.rda', version=2)
} else {
    tools::resaveRdaFiles('topoWorld.rda')
}

