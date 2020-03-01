library(oce)
xbt <- read.oce("xbt.edf", debug=10)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("adp")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(xbt, file="xbt.rda", version=2)
    tools::resaveRdaFiles('xbt.rda', version=2)
} else {
    save(xbt, file="xbt.rda")
    tools::resaveRdaFiles('xbt.rda')
}

