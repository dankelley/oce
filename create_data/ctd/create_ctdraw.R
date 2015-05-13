library(oce)
ctdraw <- read.oce("ctdraw.cnv")
ctdraw <- oce.edit(ctdraw, "startTime", ctdraw[["systemUploadTime"]],
                   reason="startTime is not in file", person="Dan Kelley")
ctdraw <- oce.edit(ctdraw, "temperature", T90fromT48(ctdraw[["temperature"]]),
                   reason="File uses old IPTS-68 convention", person="Dan Kelley")
ctdraw <- oce.edit(ctdraw, "temperatureUnit", "ITS-90",
                   reason="File uses old IPTS-68 convention", person="Dan Kelley")
save(ctdraw, file="ctdraw.rda")
tools::resaveRdaFiles("ctdraw.rda")


