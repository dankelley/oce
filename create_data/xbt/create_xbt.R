library(oce)
xbt <- read.oce("xbt.edf", debug=10)
if (TRUE || utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    save(xbt, file="xbt.rda", version=2)
    tools::resaveRdaFiles('xbt.rda', version=2)
} else {
    save(xbt, file="xbt.rda")
    tools::resaveRdaFiles('xbt.rda')
}

