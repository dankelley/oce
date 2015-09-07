library(oce)
library(testthat)
source('~/src/oce/R/cm.R')
source('~/src/oce/R/oce.R')
source('~/src/oce/R/imagep.R')

## One day of in-water data.
cm <- read.oce("/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab",
               from=840, to=840+24*60)
save(cm, file='cm.rda')
S <- cm[["salinity"]]
S1 <- swSCTp(cm)
S2 <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']],conductivityUnit=cm[['conductivityUnit']])
expect_equal(max(abs(S1-S2)), 0)
## I am not sure why these differ by 0.003PSU.
expect_less_than(max(abs(S-S1)), 0.003)
expect_less_than(max(abs(S-S2)), 0.003)
plot(cm)
summary(cm)
tools::resaveRdaFiles("cm.rda")

