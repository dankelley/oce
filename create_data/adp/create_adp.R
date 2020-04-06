library(oce)
beam <- read.oce("/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000",
                 from=as.POSIXct("2008-06-26", tz="UTC"),
                 to=as.POSIXct("2008-06-27", tz="UTC"),
                 by="60:00",
                 testing=TRUE,
                 latitude=47.88126,
                 longitude=-69.73433)
xyz <- beamToXyzAdp(beam)
adp <- xyzToEnuAdp(xyz, declination=-18.1)
adp@metadata$filename <- "(redacted)"
adp@metadata$serialNumber <- "(redacted)"
adp@metadata$headSerialNumber <- "(redacted)"
adp@metadata$deploymentName <- "(redacted)"
adp@metadata$comments <- "sample ADP file"

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("adp")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(adp, file="adp.rda", version=2)
    tools::resaveRdaFiles('adp.rda', version=2)
} else {
    save(adp, file="adp.rda")
    tools::resaveRdaFiles('adp.rda')
}


