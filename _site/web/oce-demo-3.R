library(oce)
data(sealevelHalifax)
m <- tidem(sealevelHalifax)
par(mfrow=c(2,1))
plot(sealevelHalifax, which=1)
plot(m)

