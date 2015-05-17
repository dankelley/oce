library(oce)
logger <- read.oce("/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/manufacturer/pt_rbr_011855.dat", by=600)
#patm <- loggerPatm(logger)[4] # value is 10.19443
#logger <- oceEdit(logger, action="x@data$pressure <- x@data$pressure - 10.2")
logger <- window(logger, start=as.POSIXct("2008-06-26",tz="UTC"), end=as.POSIXct("2008-06-27",tz="UTC"))
logger[["filename"]] <- "(redacted)"
logger[["serialNumber"]] <- "(redacted)"
save(logger, file="logger.rda")
library(tools)
resaveRdaFiles("logger.rda", compress="auto")

