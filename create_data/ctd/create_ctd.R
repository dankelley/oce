library(oce)
## source("~/git/oce/R/oce.R")
## source("~/git/oce/R/ctd.R")
## source("~/git/oce/R/ctd.sbe.R")
ctd <- read.oce("ctd.cnv")
ctd <- oce.edit(ctd, "startTime", ctd[["systemUploadTime"]],
                reason="startTime is not in file", person="Dan Kelley")
save(ctd, file="ctd.rda")
tools::resaveRdaFiles("ctd.rda")


