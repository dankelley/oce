library(oce)
data(adp)
e <- adp.xyz2enu(adp.beam2xyz(adp))
plot(e,which=1:3,
     adorn=expression({lines(x$data$ts$time, x$data$ts$pressure, lwd=3, col='blue')}))
