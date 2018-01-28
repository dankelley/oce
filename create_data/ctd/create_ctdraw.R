library(oce)
ctdRaw <- read.oce("BED0302.CNV")
ctdRaw <- oce.edit(ctdRaw, "startTime", ctdRaw[["systemUploadTime"]],
                   reason="startTime in file has Y2K error", person="Dan Kelley")
## ctdRaw <- oce.edit(ctdRaw, "temperature", T90fromT68(ctdRaw[["temperature"]]),
##                    reason="File uses old IPTS-68 convention", person="Dan Kelley")
## ctdRaw@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
## ctdRaw <- oce.edit(ctdRaw, reason="set metadata$units$temperature to ITS-90", person="Dan Kelley")
save(ctdRaw, file="ctdRaw.rda")
tools::resaveRdaFiles("ctdRaw.rda")
