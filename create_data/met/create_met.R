library(oce)
metFile <- download.met(6358, 2003, 9, destdir=".")
met <- read.met(metFile)
met <- oceSetData(met, "time", met[["time"]] + 4 * 3600, note="add 4h to local time to get UTC time")
save(met, file="met.rda")
tools::resaveRdaFiles("met.rda")

