options(oceEOW=TRUE)

library(oce)
data(coastlineWorld)
load("coastlineWorldPolygon.rda")
source("~/git/oce/R/map.R")
library(sf)
library(rgdal)
proj0 <- "OGC:CRS84" # longlat
proj0 <- "+proj=longlat +lat_0=-20 +datum=WGS84 +no_defs"

projFix <- function(crs) # FIXME: check sf version (?)
{
    if (grepl("+proj=ortho", crs)) {
        if (!grepl("+R=", crs))
            crs <- paste(crs, "+R=6378137")
        if (!grepl("+f=", crs))
            crs <- paste(crs, "+f=0")
    }
    crs
}

if (!interactive()) pdf("eow09.pdf", width=7, height=4, pointsize=9)
par(mar=c(3,3,1,1))

projs <- c("+proj=ortho +lon_0=-30 +lat_0=-20",
           "+proj=robin",
           "+proj=moll")[1]
for (proj in projs) {
    message(proj)
    proj <- projFix(proj)
    message(proj)
    # FIXME: create eow here, for THIS projection
    projRda <- gsub(".*=", "", gsub(" .*$", "", proj))
    projRda <- paste0("eow_", projRda, ".rda")
    message("loading \"", projRda, "\"")
    load(projRda)
    CL <- sf::sf_project(proj0, proj, coastlineWorldPolygon, keep=!TRUE, warn=FALSE)
    plot(CL, asp=1, type="n", xlab="", ylab="")
    eowLL <- sf::sf_project(proj, proj0, eow)
    eowLLp <- sf::st_make_valid(sf::st_polygon(list(outer=eowLL)))
    np <- length(pvisible)
    for (i in seq_len(np)) {
        #A <- sf::sf_project(proj0, proj, pvisible[[i]][[1]], keep=TRUE)
        A <- sf::sf_project(proj0, proj, sf::st_polygon(list(outer=coastlineWorldPolygon[[i]][[1]])), keep=TRUE, warn=FALSE)
        if (any(is.finite(A[,1]))) {
            message("i=", i, " is onworld")
            AA <- sf::st_polygon(list(outer=A))
            polygon(A[,1], A[,2], col="pink")
            v <- sf::st_intersection(AA, eowLLp)
            if (!length(v)) {
                message("i=", i, " clipped??")
            }
            #plot(v, add=TRUE, col="pink")
        } else {
            message("i=", i, " OFFworld")
        }
    }
}

