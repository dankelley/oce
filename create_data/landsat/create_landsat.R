library(oce)
landsat <- decimate(read.landsat("/data/archive/landsat/LC80080292014065LGN00"), by=100)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("landsat")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(landsat, file="landsat.rda", version=2)
    tools::resaveRdaFiles("landsat.rda", version=2)
} else {
    save(landsat, file="landsat.rda")
    tools::resaveRdaFiles("landsat.rda")
}

