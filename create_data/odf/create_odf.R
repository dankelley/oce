library(oce)
odf <- read.oce("../../inst/extdata/CTD_BCD2014666_008_1_DN.ODF.gz")
save(odf, file="odf.rda")
if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    tools::resaveRdaFiles("odf.rda", version=2)
} else {
    tools::resaveRdaFiles("odf.rda")
}

