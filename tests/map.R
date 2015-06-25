library(oce)

## UTM (numbers come from a landsat-8 image).
longitude <- -59.56594
latitude <- 45.62391
easting <- 767700.000
northing <- 5058000.000
zone <- 20

## LANDSAT_SCENE_ID = "LC82170632015124LGN00"
## ...
## CORNER_UL_LAT_PRODUCT = -3.28874
## CORNER_UL_LON_PRODUCT = -40.27900
## CORNER_UR_LAT_PRODUCT = -3.28927
## CORNER_UR_LON_PRODUCT = -38.22680
## CORNER_LL_LAT_PRODUCT = -5.39159
## CORNER_LL_LON_PRODUCT = -40.28255
## CORNER_LR_LAT_PRODUCT = -5.39245
## CORNER_LR_LON_PRODUCT = -38.22465
## CORNER_UL_PROJECTION_X_PRODUCT = 357900.000
## CORNER_UL_PROJECTION_Y_PRODUCT = -363600.000
## CORNER_UR_PROJECTION_X_PRODUCT = 585900.000
## CORNER_UR_PROJECTION_Y_PRODUCT = -363600.000
## CORNER_LL_PROJECTION_X_PRODUCT = 357900.000
## CORNER_LL_PROJECTION_Y_PRODUCT = -596100.000
## CORNER_LR_PROJECTION_X_PRODUCT = 585900.000
## CORNER_LR_PROJECTION_Y_PRODUCT = -596100.000
## ...
## UTM_ZONE = 24
longitude <- -40.27900                 # UL
latitude <- -3.28874                   # UL
easting <- 357900.000                  # UL
northing <- -363600.000                # UL 
zone <- 24


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


