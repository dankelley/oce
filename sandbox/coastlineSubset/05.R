REAL <- !TRUE # to avoid having to click whilst writing code
library(raster)
library(oce)

#' Ensure that vector starts and ends with NA
#' @param x a vector
#' @return a vector that matches x except for possible addition of NAs at endpoints
NAendpoints <- function(x) {
    if (!is.na(x[1]))
        x <- c(NA, x)
    if (!is.na(x[length(x)]))
        x <- c(x, NA)
    x
}

#' Extract coordinates from SpatialPolygons object
#' @param sp a SpatialPolygons object
#' @return a matrix with two columns, first for x, second for y
getCoords <- function(sp)
{
    stopifnot(1 == length(sp@Polygons)) # I don't know if this is always true
    sp@Polygons[[1]]@coords
}

data(coastlineWorld, package="oce")
par(mfrow=c(2, 1), mar=rep(1, 4))
plot(coastlineWorld)
mtext("Click twice to define a box", font=2, col=2)
requireNamespace("raster") # we only need a few functions and I prefer to name them
if (REAL) {
    bb <- locator(2)
    W <- min(bb$x)
    E <- max(bb$x)
    S <- min(bb$y)
    N <- max(bb$y)
} else {
    W <- -70.79
    E <- -61.79
    S <- 39.56
    N <- 53.42
    ## DK: testing effect of moving the boundary
    N <- N + 10
    E <- E + 5
    W <- W - 2
}
box <- as(raster::extent(W, E, S, N), "SpatialPolygons")
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="magenta")

cllon <- NAendpoints(coastlineWorld[["longitude"]])
cllat <- NAendpoints(coastlineWorld[["latitude"]])
na <- which(is.na(cllon))
nseg <- length(na)
col <- 1
owarn <- options("warn")$warn
options(warn=-1)
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    lon <- cllon[look]
    if (any(is.na(lon))) stop("step 1: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
    lat <- cllat[look]
    if (any(is.na(lat))) stop("step 1: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
    n <- length(lon)
    if (n < 1) stop("how can we have no data?")
    if (length(lon) > 2) {
        A <- sp::Polygon(cbind(lon, lat))
        B <- sp::Polygons(list(A), "A")
        C <- sp::SpatialPolygons(list(B))
        ##? if (iseg == 42) browser()
        i <- raster::intersect(box, C)
        if (!is.null(i)) {
            xy <- i@polygons[[1]]@Polygons[[1]]@coords
            polygon(xy[,1], xy[,2], col=col)
            col <- (col + 1) %% 5 + 1
            cat("iseg=", iseg, ": plotted in col=", col, "\n")
        } else {
            cat("iseg=", iseg, ": no intersection\n")
        }
    } else {
        cat("iseg=", iseg, ": < 3 points\n")
    }
}
options(warn=owarn)
options(warn=-1)
plot(as.coastline(c(E, W), c(S, N)), type="n")
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    lon <- cllon[look]
    if (any(is.na(lon))) stop("step 1: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
    lat <- cllat[look]
    if (any(is.na(lat))) stop("step 1: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
    n <- length(lon)
    if (n < 1) stop("how can we have no data?")
    if (length(lon) > 0) {
        A <- sp::Polygon(cbind(lon, lat))
        B <- sp::Polygons(list(A), "A")
        C <- sp::SpatialPolygons(list(B))
        ##? if (iseg == 42) browser()
        i <- raster::intersect(box, C)
        if (!is.null(i)) {
            xy <- i@polygons[[1]]@Polygons[[1]]@coords
            polygon(xy[,1], xy[,2], col=col)
            col <- (col + 1) %% 5 + 1
            cat("iseg=", iseg, ": plotted in col=", col, "\n")
        } else {
            cat("iseg=", iseg, ": no intersection\n")
        }
    } else {
        cat("iseg=", iseg, ": < 3 points\n")
    }
}
mtext(sprintf("Box: W=%.2f E=%.2f S=%.2f N=%.2f", W, E, S, N), font=2, col=2)
options(warn=owarn)

