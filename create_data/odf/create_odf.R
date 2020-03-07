library(oce)
odf <- read.oce("../../inst/extdata/CTD_BCD2014666_008_1_DN.ODF.gz")

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("odf")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(odf, file="odf.rda", version=2)
    tools::resaveRdaFiles("odf.rda", version=2)
} else {
    save(odf, file="odf.rda")
    tools::resaveRdaFiles("odf.rda")
}

