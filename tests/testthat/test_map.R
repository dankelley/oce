## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Map calculations")

if (requireNamespace("rgdal", quietly=TRUE)) {

  test_that("utm2lonlat() and lonlat2utm() on some points known from Landsat metadata", {
            longitude <- c(-40.27900, -38.22680, -40.28255, -38.22465,
                           -64.12716, -61.13114, -64.08660, -61.19836)
            latitude <- c(-3.28874,-3.28927, -5.39159, -5.39245,
                          45.66729, 45.65756, 43.53138, 43.52235)
            easting <- c(357900.000, 585900.000, 357900.000, 585900.000,
                         412200.000, 645600.000, 412200.000, 645600.000)
            northing <- c(-363600.000, -363600.000, -596100.000, -596100.000,
                          5057700.000, 5057700.000, 4820400.000, 4820400.000)
            zone <- c(rep(24, 4), rep(20, 4))
            lonlat <- utm2lonlat(easting, northing, zone, "N")
            expect_equal(lonlat$longitude, longitude, tolerance=1e-5)
            expect_equal(lonlat$latitude, latitude, tolerance=1e-5)
            utm <- lonlat2utm(lonlat$longitude, lonlat$latitude)
            expect_equal(utm$easting, easting, tolerance=1e-5)
            expect_equal(utm$northing, northing, tolerance=1e-5)
            expect_equal(utm$zone, zone, tolerance=1e-5)
})

  test_that("lonlat2utm() on image for issue 707 (corners cross zones)", {
            ## CORNER_UL_LAT_PRODUCT = 70.68271
            ## CORNER_UL_LON_PRODUCT = -54.28961
            ## CORNER_UR_LAT_PRODUCT = 70.67792
            ## CORNER_UR_LON_PRODUCT = -47.45356
            ## CORNER_LL_LAT_PRODUCT = 68.46651
            ## CORNER_LL_LON_PRODUCT = -53.96471
            ## CORNER_LR_LAT_PRODUCT = 68.46225
            ## CORNER_LR_LON_PRODUCT = -47.80372
            ## CORNER_UL_PROJECTION_X_PRODUCT = 378600.000
            ## CORNER_UL_PROJECTION_Y_PRODUCT = 7845300.000
            ## CORNER_UR_PROJECTION_X_PRODUCT = 630900.000
            ## CORNER_UR_PROJECTION_Y_PRODUCT = 7845300.000
            ## CORNER_LL_PROJECTION_X_PRODUCT = 378600.000
            ## CORNER_LL_PROJECTION_Y_PRODUCT = 7597800.000
            ## CORNER_LR_PROJECTION_X_PRODUCT = 630900.000
            ## CORNER_LR_PROJECTION_Y_PRODUCT = 7597800.000
            longitude <- c(-54.28961, -47.45356, -53.96471, -47.80372)
            latitude <- c(70.68271, 70.67792, 68.46651, 68.46225)
            northing <- c(7845300, 7845300, 7597800, 7597800)
            easting <- c(378600, 630900, 378600, 630900)
            ## in fact, the corners are in different zones 21 and 23, but the Landsat
            ## metadata indicate calculation in a single zone (22), so that is
            ## used here.
            zone <- rep(22, 4)
            utm <- lonlat2utm(longitude, latitude, zone=zone)
            ## Use tolerance to check to within a metre, surely sufficient
            ## for any purpose with landsat-8, given its pixel size.
            expect_equal(utm$northing, northing, scale=1, tolerance=0.5)
            expect_equal(utm$easting, easting, scale=1, tolerance=0.5)
})

  test_that("lonlat2map() near Cape Split", {
            ## We cannot test this on i386/windows for a while in early 2019,
            ## because rgdal on test machine is version 1.3-9, which has a
            ## coding error (relating to using an undefined variable named
            ## "nas") that will cause a failure to build.
            if (!(.Platform$OS.type == "windows" && .Platform$r_arch == "i386")) {
              ## "cs" is near Cape Split, in the Bay of Fundy
              cs <- list(longitude=-64.4966,latitude=45.3346)
              xy <- lonlat2map(cs$longitude, cs$latitude, "+proj=merc")
              cs2 <- map2lonlat(xy$x, xy$y)
              expect_equal(cs, cs2)
            }
})

  test_that("temporary tests (for rgdal/sf transition)", {
            data(coastlineWorld)
            par(mar=c(2,2,1,1))
            options(warn=3)
            mapPlot(coastlineWorld, projection="+proj=moll", col="gray")
            mapPlot(coastlineWorld, projection="+proj=ortho", col="gray")
            mapPlot(coastlineCut(coastlineWorld,-100),
                    longitudelim=c(-130,-55), latitudelim=c(35,60),
                    projection="+proj=lcc +lat_0=30 +lat_1=60 +lon_0=-100", col="gray")
            mapPlot(coastlineCut(coastlineWorld, -135),
                    longitudelim=c(-130, 50), latitudelim=c(70, 110),
                    projection="+proj=stere +lat_0=90 +lon_0=-135", col="gray")
})

}


