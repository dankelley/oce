library(oce)
ctdRaw <- read.oce("BED0302.CNV", debug=10)
ctdRaw <- oce.edit(ctdRaw, "startTime",
                   as.POSIXct(gsub("1903", "2003", format(ctdRaw[["startTime"]])), tz="UTC")+4*3600,
                   reason="file had year=1903, instead of 2003", person="Dan Kelley")
## ctdRaw <- oce.edit(ctdRaw, "temperature", T90fromT68(ctdRaw[["temperature"]]),
##                    reason="File uses old IPTS-68 convention", person="Dan Kelley")
## ctdRaw@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
## ctdRaw <- oce.edit(ctdRaw, reason="set metadata$units$temperature to ITS-90", person="Dan Kelley")
save(ctdRaw, file="ctdRaw.rda")
if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    tools::resaveRdaFiles('ctdRaw.rda', version=2)
} else {
    tools::resaveRdaFiles('ctdRaw.rda')
}

