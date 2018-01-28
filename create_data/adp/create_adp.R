library(oce)
beam <- read.oce("/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000",
                 from=as.POSIXct("2008-06-26", tz="UTC"),
                 to=as.POSIXct("2008-06-27", tz="UTC"),
                 by="60:00", 
                 testing=TRUE,
                 latitude=47.88126,
                 longitude=-69.73433)
xyz <- beamToXyzAdp(beam)
adp <- xyzToEnuAdp(xyz, declination=-18.1)
adp@metadata$filename <- "(redacted)"
adp@metadata$serialNumber <- "(redacted)"
adp@metadata$headSerialNumber <- "(redacted)"
adp@metadata$deploymentName <- "(redacted)"
adp@metadata$comments <- "sample ADP file"
adp@processingLog$time <- adp@processingLog$time[1]
adp@processingLog$value <- "(redacted)"
save(adp, file='adp.rda')
library(tools)
tools::resaveRdaFiles("adp.rda", compress="auto")

