library(oce)
rsk <- read.oce("/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/manufacturer/pt_rbr_011855.dat", by=600)
#patm <- rskPatm(rsk)[4] # value is 10.19443
#rsk <- oceEdit(rsk, action="x@data$pressure <- x@data$pressure - 10.2")
rsk <- window(rsk, start=as.POSIXct("2008-06-26",tz="UTC"), end=as.POSIXct("2008-06-27",tz="UTC"))
rsk[["filename"]] <- NA
rsk[["serialNumber"]] <- NA
save(rsk, file="rsk.rda")
library(tools)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("rsk")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save("rsk.rda", version=2)
    tools::resaveRdaFiles("rsk.rda", version=2)
} else {
    save("rsk.rda")
    tools::resaveRdaFiles("rsk.rda")
}
