library(oce)
## source("~/src/oce/R/oce.R")
## source("~/src/oce/R/ctd.R")
ctdRaw <- read.oce("BED0302.CNV")
ctdRaw <- oce.edit(ctdRaw, "startTime", ctdRaw[["systemUploadTime"]],
                   reason="startTime is not in file", person="Dan Kelley")
ctdRaw <- oce.edit(ctdRaw, "temperature", T90fromT68(ctdRaw[["temperature"]]),
                   reason="File uses old IPTS-68 convention", person="Dan Kelley")
## oce.edit() will not let us edit within 'units'
ctdRaw@metadata$units$temperature <- "ITS-90"
ctdRaw <- oce.edit(ctdRaw, reason="set metadata$units to ITS-90", person="Dan Kelley")
save(ctdRaw, file="ctdRaw.rda")
tools::resaveRdaFiles("ctdRaw.rda")
