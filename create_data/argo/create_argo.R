library(oce)
argo <- read.oce("6900388_prof.nc")
summary(argo)
save(argo, file="argo.rda")
tools::resaveRdaFiles("argo.rda")

