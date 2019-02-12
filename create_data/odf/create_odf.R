library(oce)
odf <- read.oce("../../inst/extdata/CTD_BCD2014666_008_1_DN.ODF.gz")
save(odf, file="odf.rda")
tools::resaveRdaFiles("odf.rda")


