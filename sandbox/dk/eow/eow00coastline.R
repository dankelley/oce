# eow08coastline.R: create polygon coastline (takes 0.02s if oce loaded, 1.1s otherwise)
library(oce)
data(coastlineWorld, package="oce")
lon <- c(coastlineWorld[["longitude"]], NA)
lat <- c(coastlineWorld[["latitude"]], NA)
NAs <- which(is.na(lon))
if (any(1L == diff(NAs)))
    stop("malformed coastline file (adjacent NA values)")
npoly <- length(NAs) - 1L
p <- vector("list", npoly)
for (i in seq_len(npoly)) {
    look <- seq(NAs[i] + 1L, NAs[i+1]-1L)
    LON <- lon[look]
    LAT <- lat[look]
    len <- length(look)
    # Guess how to close those polygons that are not closed
    if (LON[1] != LON[len] || LAT[1] != LAT[len]) {
        LON <- c(LON, LON[1])
        LAT <- c(LAT, LAT[1])
    }
    p[[i]] <- list(cbind(LON, LAT))
}
coastlineWorldPolygon <- sf::st_multipolygon(p)
save(coastlineWorldPolygon, file="coastlineWorldPolygon.rda")

#plot(coastlineWorldPolygon, col="pink")
