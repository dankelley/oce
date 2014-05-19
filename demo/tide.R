## Tidal analysis of Tuktoyaktuk sea level
library(oce);
data(sealevelTuktoyaktuk)
## Tidal model fitted with tidem()
tide <- tidem(sealevelTuktoyaktuk)
summary(tide)
## Extract data for plotting
time <- sealevelTuktoyaktuk[["time"]]
eta <- sealevelTuktoyaktuk[["elevation"]]
## Tidal prediction
etap <- predict(tide)
# Display results in horizontally-stacked panels
par(mfrow=c(3, 1))
oce.plot.ts(time, eta, type='l', main="Sea level [m]") # note gaps
plot(tide)
oce.plot.ts(time, eta-etap, type='l', main="Residual sea level [m]")
