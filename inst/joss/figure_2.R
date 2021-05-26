png("figure_2.png", unit="in", width=7, height=4, res=300, pointsize=9)

library(oce)                           # load library
data(sealevel)                         # use built-in example dataset
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

