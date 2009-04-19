library(oce)
sealevel <- read.oce("h275a96.dat")
stopifnot(sealevel$station.name == "Halifax")
plot(sealevel, start.time=NA, end.time=NA)
