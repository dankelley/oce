# Demo that spTransform() returns nothing, if a point cannot be projected.

library(rgdal)

# 1. Works
n <- 5
lon <- rep(30, n)
lat <- seq(-60, 60, length.out=n)
ll <- data.frame(x=lon, y=lat)
coordinates(ll) <- c("x", "y")
proj4string(ll) <- CRS("+proj=longlat")
xy1 <- spTransform(ll, CRS("+proj=ortho +lat_0=-20"))
print(xy1)

# 2. Fails on 30E, 70N. Q: is there an arg that makes spTransform() return
# Inf/NA as needed?
n <- 5
lon <- rep(30, n)
lat <- seq(-70, 70, length.out=n)
ll <- data.frame(x=lon, y=lat)
coordinates(ll) <- c("x", "y")
proj4string(ll) <- CRS("+proj=longlat")
xy2 <- spTransform(ll, CRS("+proj=ortho +lat_0=-20"), keep=TRUE)
print(xy2)

