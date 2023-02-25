library(oce)
source("~/git/oce/R/ctd.R")
data(ctd)
ctd@data$pressure[1] <- 0 # put top point at surface
par(mfrow=c(1, 2))
# 10th point S=39.9634, T=14.11931
level <- 1
S0 <- ctd[["salinity"]][level]
T0 <- ctd[["temperature"]][level]
dS <- (ctd[["SA"]] - ctd[["salinity"]])[level]
dT <- (ctd[["CT"]] - ctd[["theta"]])[level]
col <- c(2, 4, rep(1, -2+length(ctd[["pressure"]])))
plotTS(ctd, col=col, pch=20, Slim=S0+0.01*c(-1,1), Tlim=T0+0.002*c(-0.1,0.8), eos="unesco")
plotTS(ctd, col=col, pch=20, Slim=S0+dS+0.01*c(-1,1), Tlim=T0+dT+0.002*c(-0.1,0.8), eos="gsw")
ctd[["pressure"]][level]


library(oce)
CTD <- as.ctd(30, 14, 0, longitude=-63, latitude=43)
swRho(CTD, eos="unesco")[1]
swRho(CTD, eos="gsw")[1]
