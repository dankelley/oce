library(oce)
data(adp)
plot(adp, which=1:3,
     adorn=expression({lines(x[["time"]], x[["pressure"]])}))
