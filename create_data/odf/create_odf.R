library(oce)
odf <- read.oce("../../inst/extdata/CTD_BCD2014666_008_1_DN.ODF.gz")

if (utils::compareVersion(R.Version()$minor, "3.6") >= 0) {
    save(odf, file="odf.rda", version=2)
    tools::resaveRdaFiles("odf.rda", version=2)
} else {
    save(odf, file="odf.rda")
    tools::resaveRdaFiles("odf.rda")
}

