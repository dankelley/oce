## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
context("General tests")

test_that("times", {
          expect_equal(numberAsPOSIXct(719529, "matlab"), ISOdatetime(1970,1,1,0,0,0,tz="UTC"))
          expect_equal(numberAsPOSIXct(cbind(604,134351), type="gps"), as.POSIXct("2011-03-21 13:18:56",tz="UTC"))
})

test_that("matchBytes", {
          buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
          expect_equal(c(1,4), matchBytes(buf, 0xa5, 0x11))
})

test_that("time-series filtering", {
          b <- rep(1,5)/5
          a <- 1
          x <- seq(1, 4, by=0.2)
          matlab.res <- c(0.2000,0.4400,0.7200,1.0400,1.4000,1.6000,1.8000,2.0000,2.2000,
                          2.4000,2.6000,2.8000,3.0000,3.2000,3.4000,3.6000)
          expect_equal(matlab.res, oce.filter(x, a, b))
})

test_that("Magnetic field at Halifax", {
          ## test values from http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html
          expect_equal(-17.976, magneticField(-63.562,44.640,2013)$declination,tolerance=1e-3)
          expect_equal(67.562, magneticField(-63.562,44.640,2013)$inclination,tolerance=1e-3)
          expect_equal(52096, magneticField(-63.562,44.640,2013)$intensity,tolerance=1e-3)
})

test_that("Coriolis", {
          f <- coriolis(45)
          expect_equal(f, 1.031261e-4, tolerance=1e-6)
})

test_that("Gravity", {
          g <- gravity(45)
          expect_equal(g, 9.8, tolerance=1e-2)
})

test_that("numberAsPOSIXct", {
          ## Matlab times
          mt <- 7.362007209411687e5
          expect_less_than(abs(as.numeric(numberAsPOSIXct(mt, "matlab", tz="UTC")) -
                           as.numeric(as.POSIXct("2015-08-24 17:18:09", tz="UTC"))), 60)
          expect_less_than(abs(as.numeric(numberAsPOSIXct(mt, "matlab")) -
                           as.numeric(as.POSIXct("2015-08-24 17:18:09", tz="UTC"))), 60)
})
 
