library(oce)
## source("~/src/oce/R/oce.R")
## source("~/src/oce/R/ctd.R")
ctd <- read.oce("ctd.cnv")
ctd <- oce.edit(ctd, "startTime", ctd[["systemUploadTime"]],
                reason="startTime is not in file", person="Dan Kelley")
## ctd <- oce.edit(ctd, "temperature", T90fromT48(ctd[["temperature"]]),
##                 reason="File uses old IPTS-68 convention", person="Dan Kelley")
## ctd <- oce.edit(ctd, "temperatureUnit", "ITS-90",
##                 reason="File uses old IPTS-68 convention", person="Dan Kelley")
save(ctd, file="ctd.rda")
tools::resaveRdaFiles("ctd.rda")


