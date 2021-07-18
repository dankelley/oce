# test st_project()

library(sf)

n <- 20

# 1. Works
lon <- rep(30, n)
lat <- seq(-60, 60, length.out=n)
ll <- cbind(lon,lat)
xy1 <- sf::sf_project("+proj=longlat", "+proj=ortho +lat_0=-20", ll, keep=TRUE)
print(xy1)
plot(xy1[,1], xy1[,2], asp=1)

# 2. Fails on 30E, 70N. Q: is there an arg that makes spTransform() return
# Inf/NA as needed?
lon <- rep(30, n)
lat <- seq(-90, 90, length.out=n)
ll <- cbind(lon,lat)
xy2 <- sf::sf_project("+proj=longlat", "+proj=ortho +lat_0=-20", ll, keep=TRUE)
print(xy2)
points(xy2[,1], xy2[,2], col=2)
