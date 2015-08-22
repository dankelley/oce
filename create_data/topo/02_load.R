library(oce)
library(testthat)
source("~/src/oce/R/imagep.R")
# File format described at
# http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/ETOPO5.txt
f <- file("etopo5.dat", "rb")
seek(f, 0, "end", rw="read")
fileSize <- seek(f, 0, origin="start", rw="read")
n <- fileSize / 2
z <- readBin(f, integer(), size=2, endian="big", n=n)
nj <- 180*12
ni <- 360*12
z <- t(matrix(z, nrow=nj, byrow=TRUE))
expect_equal(ni, dim(z)[1])
expect_equal(nj, dim(z)[2])
latitude <- seq(90, by=-1/12, length.out=ni)
longitude <- seq(0, by=1/12, length.out=nj)
z <- z[,seq.int(nj,1)]
latitude <- latitude[seq.int(nj,1)]
imagep(longitude, latitude, z)
## OK. Now subsample to 1deg.
i <- seq.int(1, ni, 12)
j <- seq.int(1, nj, 12)
z <- z[i, j]
longitude <- longitude[i]
latitude <- latitude[j]
imagep(longitude, latitude, z)

