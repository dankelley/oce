library(oce)
sl <- read.oce(system.file("data", "h275a96.dat", package="oce"))
m <- tidem(sl)
par(mfrow=c(2,1))
plot.sealevel(sl, which=1)
plot(m)

