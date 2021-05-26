png("tide_figure.png", width=7, height=4, unit="in", res=300, pointsize=10)

library(oce)                           # load library
data(sealevel)                         # sea level in Halifax Harbour
t <- sealevel[["time"]]                # extract time
eta <- sealevel[["elevation"]]         # extract sea level
m <- tidem(sealevel)                   # fit tidal model
etaDetided <- eta - predict(m)         # de-tide observations
par(mfrow=c(2, 1))                     # set up a two-panel plot
oce.plot.ts(t, eta, xaxs="i",          # top: observed sea level
    grid=TRUE, ylab="Sea level [m]")
oce.plot.ts(t, etaDetided, xaxs="i",   # bottom: de-tided sea level
    grid=TRUE, ylab="De-tided sea level [m]")

dev.off()

