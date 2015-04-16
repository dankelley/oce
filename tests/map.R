library(oce)

## UTM (numbers come from a landsat-8 image).
longitude <- -59.56594
latitude <- 45.62391
easting <- 767700.000
northing <- 5058000.000
zone <- 20

## Projections: utm
lonlat <- utm2lonlat(easting, northing, zone, "N")
stopifnot(all.equal(lonlat$longitude, longitude, tolerance=1e-5))
stopifnot(all.equal(lonlat$latitude, latitude, tolerance=1e-5))
utm <- lonlat2utm(lonlat$longitude, lonlat$latitude)
stopifnot(all.equal(utm$easting, easting, tolerance=1e-5))
stopifnot(all.equal(utm$northing, northing, tolerance=1e-5))
stopifnot(all.equal(utm$zone, zone, tolerance=1e-5))

## "cs" is near Cape Split, in the Bay of Fundy
cs <- list(longitude=-64.4966,latitude=45.3346)
xy <- lonlat2map(cs$longitude, cs$latitude, "+proj=merc")
cs2 <- map2lonlat(xy$x, xy$y)
stopifnot(all.equal(cs, cs2, tolerance=1e-6)) # on 64bit machine can go to 1e-15


