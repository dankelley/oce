library(oce)
library(testthat)
try(source("~/src/oce/R/oce.R"))
try(source("~/src/oce/R/cm.R"))
try(source("~/src/oce/R/sw.R"))

## One day of in-water data.
cm <- read.oce("/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab",
               from=840, to=840+24*60)
message("original max cm time: ", max(cm[['time']]))
cm <- subset(cm, time < as.POSIXct("2008-06-27 00:00:00", tz="UTC"), debug=3)
message("subsetted max cm time: ", max(cm[['time']]))
save(cm, file="cm.rda")
tools::resaveRdaFiles("cm.rda")

S <- cm[["salinity"]]
S1 <- swSCTp(cm)
S2 <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']],
             conductivityUnit=as.character(cm[['conductivityUnit']]$unit))
testthat::expect_equal(max(abs(S1-S2)), 0)
## I am not sure why these differ by 0.003PSU.
testthat::expect_less_than(max(abs(S-S1)), 0.003)
testthat::expect_less_than(max(abs(S-S2)), 0.003)
tools::resaveRdaFiles("cm.rda")

