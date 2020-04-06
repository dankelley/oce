library(oce)
metFile <- download.met(id=6358, year=2003, month=9, destdir=".", type="xml")
met <- read.met(metFile)
met <- oceSetData(met, "time", met[["time"]] + 4 * 3600, note="add 4h to local time to get UTC time")

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("admet")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(met, file="met.rda", version=2)
    tools::resaveRdaFiles("met.rda", version=2)
} else {
    save(met, file="met.rda")
    tools::resaveRdaFiles("met.rda")
}

