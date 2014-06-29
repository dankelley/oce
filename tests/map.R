library(oce)

## UTM (numbers come from a landsat-8 image).
longitude <- -59.56594
latitude <- 45.62391
easting <- 767700.000
northing <- 5058000.000
zone <- 20
lonlat <- utm2lonlat(easting, northing, zone, "N")
stopifnot(all.equal(lonlat$longitude, longitude, tolerance=1e-5))
stopifnot(all.equal(lonlat$latitude, latitude, tolerance=1e-5))
utm <- lonlat2utm(lonlat$longitude, lonlat$latitude)
stopifnot(all.equal(utm$easting, easting, tolerance=1e-5))
stopifnot(all.equal(utm$northing, northing, tolerance=1e-5))
stopifnot(all.equal(utm$zone, zone, tolerance=1e-5))

## Projections
halifax <- list(latitude=44.6478, longitude=-63.5714)
projections <- c("mercator", "stereographic")
tol <- 1e-4 * 90                       # map2lonlat() uses (rel.) tol. 1e-4 by default
for (i in seq_along(projections)) {
    map <- mapproject(halifax$longitude, halifax$latitude, proj=projections[i])
    lonlat <- map2lonlat(map$x, map$y)
    ## print(abs(lonlat$longitude - halifax$longitude))
    stopifnot(tol > abs(lonlat$longitude - halifax$longitude))
    ## print(abs(lonlat$latitude - halifax$latitude))
    stopifnot(tol > abs(lonlat$latitude - halifax$latitude))
}

