## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

## References used in this file:
##
## 1. Meeus, Jean, 1982. Astronomical formulae for calculators. Willmann-Bell. Richmond VA, USA. 201 pages.
## 2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA, USA. 429 pages.

library(oce)
context("Astronomical")

RPD <- atan2(1, 1) / 45            # radians per degree
test_that("Times", {
          ## [1] chapter 3 page 24-25
          ## FIXME: previously this had the unintelligble tz="ET" but it is *exact* as is
          t <- ISOdatetime(1957, 10, 4, hour=0, min=0, sec=0, tz="UTC")+0.81*86400
          expect_equal(julianDay(t), 2436116.31, tolerance=0.01, scale=1)
          ## [1] example 15.a
          t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz="UTC")
          jd <- julianDay(t)
          jca <- julianCenturyAnomaly(jd)
          expect_equal(jd, 2443825.69, tolerance=0.01, scale=1)
          expect_equal(jca, 0.788656810, tolerance=1e-7, scale=1) # fractional error 3e-8
          ## [1] page 40
          t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz="UTC")
          expect_equal(siderealTime(t), 3.4503696, tolerance=0.0000001)
          t <- ISOdatetime(1978, 11, 13, 4, 34, 0, tz="UTC")
          expect_equal(siderealTime(t), 8.0295394, tolerance=0.0000001)
})

test_that("Moon", {
          ## [2] example 45.a (pages 312-313)
          ## Do not check too many digits, because the code does not have all terms
          ## in formulae.  (Note: this also tests eclipticalToEquatorial)
          t <- ISOdatetime(1992, 04, 12, 0, 0, 0, tz="UTC") 
          m <- moonAngle(t, 0, 0) # lat and lon arbitrary
          expect_less_than(abs(m$lambda - 133.162659), 0.02)
          expect_less_than(abs(m$beta - -3.229127), 0.001)
          ##expect_equal(abs(m$obliquity - 23.440636) < 0.001)
          expect_less_than(abs(m$rightAscension - 134.688473), 0.02)
          expect_less_than(abs(m$declination - 13.768366), 0.01)
          expect_less_than(abs(m$diameter - 0.991990), 0.0001)
          expect_less_than(abs(m$distance - 368405.6), 20)
          ## moon illuminated fraction [1] ex 31.b page 156
          illfrac <- (1 + cos(RPD * 105.8493)) / 2
          expect_equal(moonAngle(ISOdatetime(1979,12,25,0,0,0,tz="UTC"),0,0)$illuminatedFraction,illfrac,tolerance=0.001)
})
