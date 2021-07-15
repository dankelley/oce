library(rgdal)
proj <- "+proj=ortho +lat_0=-20 +datum=WGS84 +no_defs"

grid <- expand.grid(lon=seq(-180,180,2.5), lat=seq(-90,90,2.5))
lonlat <- cbind(grid$lon, grid$lat)
xy <- project(lonlat, proj)
LONLAT <- project(xy, proj, inv=TRUE)
ok <- is.finite(LONLAT[,1]) & is.finite(LONLAT[,2])

par(mar=c(3,3,1,1), mgp=c(2,0.7,0))
plot(xy[,1]/1e3, xy[,2]/1e3, asp=1,
     xlab="Easting Coordinate [km]", ylab="Northing Coordinate [km]",
     pch=20, cex=0.3, col=ifelse(ok, "red", "blue"))
grid()

