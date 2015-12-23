library(oce)
try(source("~/src/oce/R/adv.R"))
try(source("~/src/oce/R/adv.sontek.R"))
try(source("~/src/oce/R/adv.nortek.R"))
load("/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/r/m05_vector_enu.rda")
adv <- window(m05VectorEnu, as.POSIXct("2008-07-01 00:00:00", tz="UTC"), as.POSIXct("2008-07-01 00:01:00",tz="UTC"))
adv@metadata$units <- list() # the original object didn't have units
adv@metadata$filename <- "(file name redacted)"
adv@metadata$serialNumber <- "(serial number redacted)"
adv@metadata$headSerialNumber <- "(head serial number redacted)"
adv@metadata$deploymentName <- "(deployment name redacted)"
adv@metadata$comments <- "sample ADV file"
save(adv, file='adv.rda')
library(tools)
tools::resaveRdaFiles("adv.rda", compress="auto")
