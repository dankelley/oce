library(oce)
context("Geodesics")
test_that("forward=inverse", {
          data(section)
          lon <- section[["longitude", "byStation"]]
          lat<- section[["latitude", "byStation"]]
          xy <- geodXy(lon, lat, longitudeRef=lon[1], latitudeRef=lat[1])
          LONLAT <- geodXyInverse(xy$x, xy$y, longitudeRef=lon[1], latitudeRef=lat[1])
          err <- mean(sqrt((lon-LONLAT$longitude)^2+(lat-LONLAT$latitude)^2))
          expect_equal(lon, LONLAT$longitude, tolerance=1e-5) # 1m criterion; basin-wide data
          expect_equal(lat, LONLAT$latitude, tolerance=1e-5) # 1m criterion; basin-wide data
          ## Test NA functioning
          lonNA <- lon
          lonNA[100] <- NA
          latNA <- lat
          latNA[100] <- NA
          xyNA <- geodXy(lonNA, latNA, longitudeRef=lon[1], latitudeRef=lat[1])
          expect_true(is.na(xyNA$x[100]))
          expect_true(is.na(xyNA$y[100]))
          LONLATNA <- geodXyInverse(xyNA$x, xyNA$y, longitudeRef=lon[1], latitudeRef=lat[1])
          expect_true(is.na(LONLATNA$longitude[100]))
          expect_true(is.na(LONLATNA$latitude[100]))
          i <- seq_along(lon)
          expect_equal(xyNA$x[-100], xy$x[-100])
          expect_equal(xyNA$y[-100], xy$y[-100])
          expect_equal(LONLATNA$longitude[-100], lon[-100], tolerance=1e-5)
          expect_equal(LONLATNA$latitude[-100], lat[-100], tolerance=1e-5)

})

test_that("geodDist()", {
          ## Test for same values after rewriting the C code in C++.
          d <- geodDist(10, 45, 10, 46)
          expect_equal(d, 111.1415, tolerance=1e-4)
          data(section)
          expect_equal(head(geodDist(section)),
                       c(0.00000000, 19.32428588, 41.97088286, 55.38400049,
                         77.12095421, 97.43227620))
          expect_equal(head(geodDist(section, alongPath=TRUE)),
                       c(0.00000000, 19.32428588, 41.98024777, 55.45123647,
                         77.19516929, 110.46625932))
          expect_equal(head(geodDist(section, alongPath=FALSE)),
                       c(0.00000000, 19.32428588, 41.97088286, 55.38400049,
                         77.12095421, 97.43227620))
          expect_equal(tail(geodDist(section, alongPath=FALSE)),
                       c(5587.849412, 5599.757509, 5610.041722, 5621.557394,
                         5629.160056, 5633.492666))
})

test_that("geodDist() with vector locations", {
          lon1 <- c(-63, -63)
          lat1 <- c(45, 46)
          d1 <- geodDist(lon1, lat1, alongPath=FALSE)
          expect_equal(d1, c(0, 111.141548474209))
          expect_equal(d1, geodDist(lon1, lat1, lon1[1], lat1[1], alongPath=FALSE))
})

