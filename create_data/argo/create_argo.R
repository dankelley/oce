library(oce)
argo <- read.oce("6900388_prof.nc")
summary(argo)

if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    save(argo, file="argo.rda", version=2)
    tools::resaveRdaFiles('argo.rda', version=2)
} else {
    save(argo, file="argo.rda")
    tools::resaveRdaFiles('argo.rda')
}

