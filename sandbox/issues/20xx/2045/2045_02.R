# sandbox/issues/20xx/2045/2045_2.R
#
# Map diff between density computed with unesco and gsw methods.
#
# See https://github.com/dankelley/oce/issues/2045

library(oce)
data(coastlineWorld)
lon <- seq(-180, 180, 1)
lat <- seq(-90, 90, 1)
grid <- expand.grid(lon=lon, lat=lat)
n <- length(grid$lon)

S <- rep(30.0, n)
T <- rep(14.0, n)
p <- rep(0.0, n)

CTD <- as.ctd(S, T, p, longitude=grid$lon, latitude=grid$lat)
rhoU <- swRho(CTD, eos="unesco") - 1000
rhoUm <- matrix(rhoU, nrow=length(lon))
rhoG <- swRho(CTD, eos="gsw") - 1000
rhoGm <- matrix(rhoG, nrow=length(lon))
diffm <- rhoGm - rhoUm

eps <- 0.02
zlim <- quantile(abs(diffm), c(eps, 1-eps), na.rm=TRUE)
cm <- colormap(zlim=zlim)

layout(matrix(1:2, ncol=1), heights=c(0.7, 0.3))

imagep(lon, lat, diffm, colormap=cm, asp=1)
polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col="gray")
mtext(expression(rho[teos10]-rho[eos80]))
hist(diffm, xlab=expression(rho[teos10]-rho[eos80]), main="")
