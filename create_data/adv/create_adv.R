library(oce)
m05VectorEnu <- NULL # prevent code-analysis warning; the load() actually defines this.
load("/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_enu.rda")
adv <- window(m05VectorEnu, as.POSIXct("2008-07-01 00:00:00", tz="UTC"), as.POSIXct("2008-07-01 00:01:00",tz="UTC"))
adv@metadata$flags <- list() # the original object didn't have flags
adv@metadata$filename <- "(file name redacted)"
adv@metadata$serialNumber <- "(serial number redacted)"
adv@metadata$headSerialNumber <- "(head serial number redacted)"
adv@metadata$deploymentName <- "(deployment name redacted)"
adv@metadata$comments <- "sample ADV file"
adv@metadata$units$v=list(unit=expression(m/s), scale="")
adv@metadata$units$pressure=list(unit=expression(dbar), scale="")
adv@metadata$units$headingSlow=list(unit=expression(degree), scale="")
adv@metadata$units$pitchSlow=list(unit=expression(degree), scale="")
adv@metadata$units$rollSlow=list(unit=expression(degree), scale="")
adv@metadata$units$temperatureSlow=list(unit=expression(degree*C), scale="")
adv@metadata$numberOfCells <- NULL # issue 1381
save(adv, file="adv.rda")
if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    tools::resaveRdaFiles("adv.rda", version=2)
} else {
    tools::resaveRdaFiles("adv.rda")
}

