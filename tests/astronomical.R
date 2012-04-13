## References used in this file:
##
## Meeus, Jean, 1982.  Astronomical formuae for Calculators.
## Willmann-Bell. Richmond VA, USA. 201 pages.

library(oce)

## Meeus (1982 chapter 3 page 24-25)
t <- ISOdatetime(1957, 10, 4, hour=0, min=0, sec=0, tz="ET")+0.81*86400
stopifnot(all.equal(julianDay(t), 2436116.31, 0.01))

## Meeus (1982 example 15.a)
t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz="UTC")
jd <- julianDay(t)
jca <- julianCenturyAnomaly(jd)
stopifnot(all.equal(jd, 2443825.69, 0.01))
stopifnot(all.equal(jca, 0.788656810, 1e-7)) # fractional error 3e-8

## Meeus (1982 p40) sidereal time
t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz="UTC")
stopifnot(all.equal(siderealTime(t), 3.4503696, 0.0000001))
t <- ISOdatetime(1978, 11, 13, 4, 34, 0, tz="UTC")
stopifnot(all.equal(siderealTime(t), 8.0295394, 0.0000001))

## Example 45.a (pages 312-313) Meeus [1991]
## Do not check too many digits, because the code does not have all terms
## in formulae.
t <- ISOdatetime(1992, 04, 12, 0, 0, 0, tz="UTC") 
m <- moonAngle(t, 0, 0) # lat and lon arbitrary
stopifnot(all.equal(m$lambda, 133.162659, 0.010000))
stopifnot(all.equal(m$beta, -3.229127, 0.010000))
stopifnot(all.equal(m$pi, 0.991990, 0.000100))
stopifnot(all.equal(m$distance, 368405.6, 0.1)) # "Delta" on p 313
stopifnot(all.equal(m$epsilon, 23.440636, 0.000100))
stopifnot(all.equal(m$rightAscension, 134.388473, 0.010000))
stopifnot(all.equal(m$declination, 13.768366, 0.001000))

