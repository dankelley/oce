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

# Try an offworld point
N <- 30
for (X in seq(-7000, 7000, 500)) {
    off <- 1e3*cbind(rep(X, N), seq(-6500,6500,length.out=N))
    #points(off[,1], off[,2])
    rm(offLONLAT)
    offLONLAT <- try(project(off, proj, inv=TRUE), silent=TRUE)
    ok <- is.finite(offLONLAT[,1]) & is.finite(offLONLAT[,2])
    points(off[,1]/1e3, off[,2]/1e3, pch=20, col=ifelse(ok, 2, 4), cex=1)
}
data.frame(off, ok)
