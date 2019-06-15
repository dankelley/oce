library(oce)
library(sp)
if (!interactive()) pdf("01.pdf")
par(mfrow=c(2, 2))
## 1. Q: can we recognize points in polygon?
data(coastlineWorld)
px <- c(-70, -70, -45, -45)
py <- c(35, 52, 52, 35)
plot(coastlineWorld, clongitude=mean(px), clatitude=mean(py), span=20000)
polygon(px, py, col="transparent", border="red")
lon <- coastlineWorld[["longitude"]]
lat <- coastlineWorld[["latitude"]]
pip <- 1 == sp::point.in.polygon(lon, lat, px, py)
points(lon, lat, col=ifelse(pip, 'red', 'blue'), pch=20, cex=1/3)
## A: yes, looks good

## 2. Go through segments (islands and continents)
plot(coastlineWorld, clongitude=mean(px), clatitude=mean(py), span=20000)
polygon(px, py, col="transparent", border="red")
LAT <- LON <- NULL # will store final result
lats <- lons <- NULL # will store individual segments
na <- which(is.na(lon))
nseg <- length(na)
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    pip <- 1 == sp::point.in.polygon(lon[look], lat[look], px, py)
    if (any(pip == 1)) {
        LON <- c(LON, NA, lon[look])
        LAT <- c(LAT, NA, lat[look])
    }
}
lines(LON, LAT, lwd=2, col="darkgreen")

label <- sprintf("shortening factor: %.3f", length(LON)/length(lon))

cl <- as.coastline(longitude=LON, latitude=LAT)
plot(cl, clongitude=mean(px), clatitude=mean(py), span=20000)
polygon(px, py, col="transparent", border="red")
mtext(label, line=0.5, cex=3/4)

plot(cl, clongitude=mean(px), clatitude=mean(py), span=4000)
polygon(px, py, col="transparent", border="red")
mtext(label, line=0.5, cex=3/4)

if (!interactive()) dev.off()
