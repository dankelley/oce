# Test of geodesic and earth-related calculations
library(oce)

d <- geod.dist(45, 10, 46, 10)
stopifnot(all.equal.numeric(d, 111.1415, 1e-4))

f <- coriolis(45)
stopifnot(all.equal.numeric(f, 1.028445e-4, 1e-6))

g <- gravity(45)
stopifnot(all.equal.numeric(g, 9.8, 1e-2))

d <- sw.depth(10000, 30) 
stopifnot(all.equal.numeric(d, 9712.653, 1e-6))
