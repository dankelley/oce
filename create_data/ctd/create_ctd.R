library(oce)
ctd <- read.oce("ctd.cnv", debug=10)
ctd <- oce.edit(ctd, "startTime",
                as.POSIXct(gsub("1903", "2003", format(ctd[["startTime"]])), tz="UTC")+4*3600,
                reason="file had year=1903, instead of 2003", person="Dan Kelley")
##ctd <- oce.edit(ctd, "temperature", T90fromT68(ctd[["temperature"]]),
##                reason="File uses old IPTS-68 convention", person="Dan Kelley")
##ctd@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
##ctd <- oce.edit(ctd, reason="set metadata$units$temperature to ITS-90", person="Dan Kelley")

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("ctd")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(ctd, file="ctd.rda", version=2)
    tools::resaveRdaFiles('ctd.rda', version=2)
} else {
    message("saving without version=2 since R version is earlier than 3.6.0")
    save(ctd, file="ctd.rda")
    tools::resaveRdaFiles('ctd.rda')
}

