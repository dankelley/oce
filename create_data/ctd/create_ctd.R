library(oce)
## source("~/src/oce/R/oce.R")
## source("~/src/oce/R/ctd.R")
ctd <- read.oce("ctd.cnv")
ctd <- oce.edit(ctd, "startTime", ctd[["systemUploadTime"]],
                reason="startTime is not in file", person="Dan Kelley")
save(ctd, file="ctd.rda")
tools::resaveRdaFiles("ctd.rda")


