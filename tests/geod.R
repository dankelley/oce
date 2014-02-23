# Test of geodesic and earth-related calculations
library(oce)

d <- geodDist(10, 45, 10, 46)
stopifnot(all.equal.numeric(d, 111.1415, tolerance=1e-4))

xy <- geodXy(0,46,0,45)/1000
stopifnot(all.equal(xy$x,     0, tolerance=1e-4))
stopifnot(all.equal(xy$y, 111.1, tolerance=1e-3))

f <- coriolis(45)
stopifnot(all.equal.numeric(f, 1.031261e-4, tolerance=1e-6))

g <- gravity(45)
stopifnot(all.equal.numeric(g, 9.8, tolerance=1e-2))

d <- swDepth(10000, 30)
stopifnot(all.equal.numeric(d, 9712.653, tolerance=1e-6))
