library(oce)
data(sealevel)
stopifnot(all.equal.numeric(sealevel[["latitude"]],   44.666667))
stopifnot(all.equal.numeric(sealevel[["longitude"]], -63.583333))
stopifnot(all.equal.numeric(sealevel[["stationNumber"]], 490))
stopifnot(all.equal.numeric(sealevel[["GMTOffset"]], 0))

