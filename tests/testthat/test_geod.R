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
})

