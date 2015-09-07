library(oce)
source('~/src/oce/R/cm.R')
source('~/src/oce/R/oce.R')
cm <- read.oce("/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab",
               from=840, to=840+24*60)
summary(cm)
save(cm, file='cm.rda')
tools::resaveRdaFiles("cm.rda")

