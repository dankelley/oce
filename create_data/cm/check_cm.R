library(oce)

## One day of in-water data.
cm <- read.oce("/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab",
               from=840, to=840+24*60)
cm <- subset(cm, time < as.POSIXct("2008-06-27 00:00:00", tz="UTC"))

S <- cm[["salinity"]]
S1 <- swSCTp(cm)
S2 <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']],
             conductivityUnit=as.character(cm[['conductivityUnit']]$unit))
expect_equal(max(abs(S1-S2)), 0)
## I am not sure why these differ by 0.003PSU.
expect_lt(max(abs(S-S1)), 0.003)
expect_lt(max(abs(S-S2)), 0.003)

