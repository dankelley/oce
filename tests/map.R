library(oce)
## These numbers come from a landsat-8 image.

longitude <- -59.56594
latitude <- 45.62391
easting <- 767700.000 / 1000           # convert to km
northing <- 5058000.000 / 1000         # convert to km
zone <- 20

lonlat <- utm2lonlat(easting, northing, zone, "N")
stopifnot(all.equal(lonlat$longitude, longitude, tolerance=1e-5))
stopifnot(all.equal(lonlat$latitude, latitude, tolerance=1e-5))
utm <- lonlat2utm(lonlat$longitude, lonlat$latitude)
stopifnot(all.equal(utm$easting, easting, tolerance=1e-5))
stopifnot(all.equal(utm$northing, northing, tolerance=1e-5))
stopifnot(all.equal(utm$zone, zone, tolerance=1e-5))

