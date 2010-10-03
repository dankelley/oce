library(oce)
data(adp)
plot(adp, which=1:3,
     adorn=expression({lines(x$data$ts$time, x$data$ts$pressure, lwd=3, col='blue')}))
