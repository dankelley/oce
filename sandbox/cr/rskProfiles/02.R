rm(list=ls())
library(oce)
source('01.R')

d <- as.ctd(read.oce('/data/archive/rbr/2019_Spring_AZMP/rbr/rsk/060328_20190415_1840.rsk'))

prof <- detectProfiles(d)

dd <- ctdFindProfiles(d, breaks=prof$index)[-1]
## If we want only up/down:
down <- dd[prof$event == 1]
up <- dd[prof$event == 2]
