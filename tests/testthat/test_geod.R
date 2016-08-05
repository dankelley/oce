library(oce)
context("Geodesics")
test_that("forward=inverse", {
          data(section)
          lon <- section[["longitude", "byStation"]]
          lat<- section[["latitude", "byStation"]]
          xy <- geodXy(lon, lat)
          LONLAT <- geodXyInverse(xy$x, xy$y)
          err <- mean(sqrt((lon-LONLAT$longitude)^2+(lat-LONLAT$latitude)^2))
          expect_equal(lon, LONLAT$longitude, tolerance=1e-5) # 1m criterion; basin-wide data
          expect_equal(lat, LONLAT$latitude, tolerance=1e-5) # 1m criterion; basin-wide data
          ## Test NA functioning
          lonNA <- lon
          lonNA[100] <- NA
          latNA <- lat
          latNA[100] <- NA
          xyNA <- geodXy(lonNA, latNA)
          expect_true(is.na(xyNA$x[100]))
          expect_true(is.na(xyNA$y[100]))
          LONLATNA <- geodXyInverse(xyNA$x, xyNA$y)
          expect_true(is.na(LONLATNA$longitude[100]))
          expect_true(is.na(LONLATNA$latitude[100]))
          i <- seq_along(lon)
          expect_equal(xyNA$x[-100], xy$x[-100])
          expect_equal(xyNA$y[-100], xy$y[-100])
          expect_equal(LONLATNA$longitude[-100], lon[-100], tolerance=1e-5)
          expect_equal(LONLATNA$latitude[-100], lat[-100], tolerance=1e-5)

})

