library(oce)
landsat <- decimate(read.landsat("/data/archive/landsat/LC80080292014065LGN00"), by=100)

if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    save(landsat, file="landsat.rda", version=2)
    tools::resaveRdaFiles("landsat.rda", version=2)
} else {
    save(landsat, file="landsat.rda")
    tools::resaveRdaFiles("landsat.rda")
}

