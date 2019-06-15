library(oce)
metFile <- download.met(6358, 2003, 9, destdir=".")
met <- read.met(metFile)
met <- oceSetData(met, "time", met[["time"]] + 4 * 3600, note="add 4h to local time to get UTC time")

if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    save(met, file="met.rda", version=2)
    tools::resaveRdaFiles("met.rda", version=2)
} else {
    save(met, file="met.rda")
    tools::resaveRdaFiles("met.rda")
}

