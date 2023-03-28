# sandbox/issues/20xx/2044/2044_02.R
#
# Mimic the TS plot that led to issue 2043.
#
# see https://github.com/dankelley/oce/issues/2044
library(oce)
ctd <- read.oce("d201211_0047.cnv")

Slim <- c(34.8327, 34.8355)
Slim <- c(34.8327, 34.8328)
Tlim<- c(0.8638, 0.879)
Tlim<- c(0.8638, 0.8640)

plotTS(ctd, Slim=Slim, Tlim=Tlim, eos="unesco", type="o", cex=0.6, gridIsopycnals=NULL)
usr <- par("usr")
NS <- 50
NT <- 60
SS <- seq(usr[1], usr[2], length.out=NS)
TT <- seq(usr[3], usr[4], length.out=NT)
grid <- expand.grid(S=SS, T=TT, KEEP.OUT.ATTRS=FALSE)
rho <- swRho(grid$S, grid$T, rep(0.0, length.out=NS*NT), eos="unesco")
dim(rho) <- c(NS, NT)
contour(SS, TT, rho, add=TRUE, col=2, drawlabels=FALSE, lty=2)
