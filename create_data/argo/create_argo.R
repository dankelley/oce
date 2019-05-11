library(oce)
argo <- read.oce("6900388_prof.nc")
summary(argo)
save(argo, file="argo.rda")
if (utils::compareVersion(R.Version()$minor, '3.6') >= 0) {
    tools::resaveRdaFiles('argo.rda', version=2)
} else {
    tools::resaveRdaFiles('argo.rda')
}

