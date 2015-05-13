library(oce)
ctdRaw <- read.oce("BED0302.CNV")
ctdRaw <- oce.edit(ctdRaw, "startTime", ctdRaw[["systemUploadTime"]],
                   reason="startTime is not in file", person="Dan Kelley")
ctdRaw <- oce.edit(ctdRaw, "temperature", T90fromT48(ctdRaw[["temperature"]]),
                   reason="File uses old IPTS-68 convention", person="Dan Kelley")
ctdRaw <- oce.edit(ctdRaw, "temperatureUnit", "ITS-90",
                   reason="File uses old IPTS-68 convention", person="Dan Kelley")
save(ctdRaw, file="ctdRaw.rda")
tools::resaveRdaFiles("ctdRaw.rda")


