library(oce)
## try(source("../../R/oce.R"))
## try(source("../../R/ctd.R"))
## try(source("../../R/odf.R"))
odf <- read.oce("../../inst/extdata/CTD_BCD2014666_008_1_DN.ODF")
save(odf, file="odf.rda")
tools::resaveRdaFiles("odf.rda")


