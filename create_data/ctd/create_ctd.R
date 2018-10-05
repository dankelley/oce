library(oce)
ctd <- read.oce("ctd.cnv")
ctd <- oce.edit(ctd, "startTime", ctd[["systemUploadTime"]],
                reason="startTime in file has Y2K error", person="Dan Kelley")
##ctd <- oce.edit(ctd, "temperature", T90fromT68(ctd[["temperature"]]),
##                reason="File uses old IPTS-68 convention", person="Dan Kelley")
##ctd@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
##ctd <- oce.edit(ctd, reason="set metadata$units$temperature to ITS-90", person="Dan Kelley")
save(ctd, file="ctd.rda")
tools::resaveRdaFiles("ctd.rda")
