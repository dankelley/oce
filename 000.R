library(oce)
#> Loading required package: gsw
data(section)
n <- length(section[['station']][[100]][['pressure']])
section[['station']][[100]][['pressure']] <- rep(NA, n)
plot(section, which='temperature', ztype='image', xtype='longitude', xlim=c(-73, 0))

