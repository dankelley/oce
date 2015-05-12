library(oce)
ctd <- read.oce("ctd.cnv")
ctd[["startTime"]] <- ctd[["systemUploadTime"]]
save(ctd, file="ctd.rda")
tools::resaveRdaFiles("ctd.rda")


