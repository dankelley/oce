library(oce)
source("~/src/oce/R/oce.R")
source("~/src/oce/R/rsk.R")
rsk <- read.oce("/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/manufacturer/pt_rbr_011855.dat", by=600)
#patm <- rskPatm(rsk)[4] # value is 10.19443
#rsk <- oceEdit(rsk, action="x@data$pressure <- x@data$pressure - 10.2")
rsk <- window(rsk, start=as.POSIXct("2008-06-26",tz="UTC"), end=as.POSIXct("2008-06-27",tz="UTC"))
rsk[["filename"]] <- NA
rsk[["serialNumber"]] <- NA
save(rsk, file="rsk.rda")
library(tools)
tools::resaveRdaFiles("rsk.rda", compress="auto")
summary(rsk)
