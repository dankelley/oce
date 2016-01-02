library(oce)
## try(source("~/src/oce/R/oce.R"))
## try(source("~/src/oce/R/argo.R"))
argo <- read.oce("6900388_prof.nc")
save(argo, file="argo.rda")
tools::resaveRdaFiles("argo.rda")

