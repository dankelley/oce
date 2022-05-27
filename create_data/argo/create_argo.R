library(oce)
argo <- read.oce("6900388_prof.nc", debug=3)
summary(argo)

## Save in version 2, because otherwise users with R 3.5.x and earlier will not
## be able to use data("argo")
if (utils::compareVersion(paste0(R.Version()$major, ".", R.Version()$minor), '3.6.0') >= 0) {
    message("saving with version=2 since R version is 3.6.0 or later")
    save(argo, file="argo.rda", version=2)
    tools::resaveRdaFiles('argo.rda', version=2)
} else {
    save(argo, file="argo.rda")
    tools::resaveRdaFiles('argo.rda')
}

