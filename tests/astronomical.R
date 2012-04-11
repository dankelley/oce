## References used in this file:
##
## Meeus, Jean, 1982.  Astronomical formuae for Calculators.
## Willmann-Bell. Richmond VA, USA. 201 pages.

library(oce)

## Meeus (1982 ?)
t <- ISOdatetime(1957, 10, 4, hour=0, min=0, sec=0, tz="ET")+0.81*86400
stopifnot(all.equal(julianDay(t), 2436116.31, 0.01))

## Meeus (1982 example 15.a)
t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz="UTC")
jd <- julianDay(t)
jca <- julianCenturyAnomaly(jd)
stopifnot(all.equal(jd, 2443825.69, 0.01))
stopifnot(all.equal(jca, 0.788656810, 1e-7)) # fractional error 3e-8

