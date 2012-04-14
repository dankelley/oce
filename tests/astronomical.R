## References used in this file:
##
## 1. Meeus, Jean, 1982. Astronomical formulae for calculators. Willmann-Bell. Richmond VA, USA. 201 pages.
## 2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA, USA. 429 pages.

library(oce)
RPD <- atan2(1, 1) / 45            # radians per degree

## [1] chapter 3 page 24-25
t <- ISOdatetime(1957, 10, 4, hour=0, min=0, sec=0, tz="ET")+0.81*86400
stopifnot(all.equal(julianDay(t), 2436116.31, 0.01))

## [1] example 15.a
t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz="UTC")
jd <- julianDay(t)
jca <- julianCenturyAnomaly(jd)
stopifnot(all.equal(jd, 2443825.69, 0.01))
stopifnot(all.equal(jca, 0.788656810, 1e-7)) # fractional error 3e-8

## [1] page 40
t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz="UTC")
stopifnot(all.equal(siderealTime(t), 3.4503696, 0.0000001))
t <- ISOdatetime(1978, 11, 13, 4, 34, 0, tz="UTC")
stopifnot(all.equal(siderealTime(t), 8.0295394, 0.0000001))

## [2] example 45.a (pages 312-313)
## Do not check too many digits, because the code does not have all terms
## in formulae.  (Note: this also tests eclipticalToEquatorial)
t <- ISOdatetime(1992, 04, 12, 0, 0, 0, tz="UTC") 
m <- moonAngle(t, 0, 0) # lat and lon arbitrary
stopifnot(all.equal(m$lambda, 133.162659, 0.000100))
stopifnot(all.equal(m$beta, -3.229127, 0.010000))
stopifnot(all.equal(m$pi, 0.991990, 0.000100))
stopifnot(all.equal(m$distance, 368405.6, 0.1)) # "Delta" on p 313
##stopifnot(all.equal(m$epsilon, 23.440636, 0.000100))
stopifnot(all.equal(m$rightAscension, 134.388473, 0.010000))
stopifnot(all.equal(m$declination, 13.768366, 0.001000))
## moon illuminated fraction [1] ex 31.b page 156
illfrac <- (1 + cos(RPD * 105.8493)) / 2
stopifnot(all.equal(moonAngle(ISOdatetime(1979, 12, 25, 0, 0, 0, tz="UTC"), 0, 0)$illuminatedFraction, illfrac, 0.001))


