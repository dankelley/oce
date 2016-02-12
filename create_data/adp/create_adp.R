library(oce)
## try(source("~/src/oce/R/oce.R"))
## try(source("~/src/oce/R/adp.R"))
## try(source("~/src/oce/R/adp.rdi.R"))
beam <- read.oce("/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000",
                 from=as.POSIXct("2008-06-26", tz="UTC"),
                 to=as.POSIXct("2008-06-27", tz="UTC"),
                 by="60:00", 
                 latitude=47.88126,
                 longitude=-69.73433)
xyz <- beamToXyzAdp(beam)
adp <- xyzToEnuAdp(xyz, declination=-18.1)
adp@metadata$filename <- "(file name redacted)"
adp@metadata$serialNumber <- "(serial number redacted)"
adp@metadata$headSerialNumber <- "(head serial number redacted)"
adp@metadata$deploymentName <- "(deployment name redacted)"
adp@metadata$comments <- "sample ADP file"
adp@processingLog$time <- adp@processingLog$time[1]
adp@processingLog$value <- "(data processing redacted)"
save(adp, file='adp.rda')
library(tools)
tools::resaveRdaFiles("adp.rda", compress="auto")

