options(oceEOW=TRUE)

# iii is trying to see why S Amer is messed up
#iii <- c(6,7,9,11,15,34,35,36,72,73,80,81,90,92,102,104,112,113,119,120,122,146,185,197,198,211,215,238,244,249,261,269,281)
iii <- c(6,7,34,35,36,72,73,80,81,90,92,102,104,119,120,122,146,185,197,198,211,215,249,261,269,281)
if (!interactive()) pdf("eow03.pdf", width=7, height=4, pointsize=9)
par(mar=rep(1,4))
library(oce)
#source("~/git/oce/R/map.R")
library(sf)
library(rgdal)
data(coastlineWorld)

projs <- c("+proj=ortho +lon_0=-30 +lat_0=-20",
           "+proj=robin",
           "+proj=moll")
for (proj in projs) {
    message(proj)
    lon <- coastlineWorld[["longitude"]]
    lat <- coastlineWorld[["latitude"]]
    mapPlot(lon, lat, proj=proj, col="lightgray")
    # eow is stored in an rda file created by eow02rgdal.R
    projRda <- gsub(".*=", "", gsub(" .*$", "", proj))
    projRda <- paste0("eow_", projRda, ".rda")
    message("loading \"", projRda, "\"")
    load(projRda)
    LL <- eow[[1]]
    eowLL <- rgdal::project(LL, proj, inv=TRUE)
    mapLines(eowLL[,1], eowLL[,2], col=4)
    # make eow polygon be valid (e.g. not self-intersecting)
    eow2tmp <- sf::st_polygon(list(cbind(eowLL[,1], eowLL[,2])))
    eow2 <- st_make_valid(eow2tmp)
    stopifnot(st_is_valid(eow2))

    NAs <- which(is.na(lon))
    par(mar=c(3,3,1,1))
    #par(mfrow=c(1,2))
    plot(coastlineWorld)
    plot(eow2, add=TRUE)
    # 289
    for (i in seq(1L, length(NAs)-1L)) {
        look <- seq(NAs[i]+1, NAs[i+1]-1)
        lonPortion <- lon[look]
        latPortion <- lat[look]
        n <- length(look)
        if (lonPortion[1] != lonPortion[n] || latPortion[1] != latPortion[n]) {
            lonPortion <- c(lonPortion, lonPortion[1])
            latPortion <- c(latPortion, latPortion[1])
        }
        p <- sf::st_polygon(list(cbind(lonPortion, latPortion)))
        pvisible <- sf::st_intersection(p, eow2)
        #if (min(latPortion) > -70 && max(latPortion) < 20 && -90 <= min(lonPortion) && max(lonPortion) < -30) {
        plot(pvisible, add=TRUE, col=if (i %in% iii) "red" else "blue")
    }

    # the real test
    par(mar=rep(1, 4))

    mapPlot(coastlineWorld, proj=proj, type="n")
    for (i in seq(1L, length(NAs)-1L)) {
        look <- seq(NAs[i]+1L, NAs[i+1]-1L)
        lonPortion <- lon[look]
        latPortion <- lat[look]
        n <- length(look)
        if (lonPortion[1] != lonPortion[n] || latPortion[1] != latPortion[n]) {
            lonPortion <- c(lonPortion, lonPortion[1])
            latPortion <- c(latPortion, latPortion[1])
        }
        if (any(is.na(lonPortion))) stop("lonPortion has 1 or more NA values")
        if (any(is.na(latPortion))) stop("latPortion has 1 or more NA values")
        p <- sf::st_polygon(list(cbind(lonPortion, latPortion)))
        pvisible <- sf::st_intersection(p, eow2)
        if (length(pvisible)) {
            #if (i %in% iii) message("i=",i, " length=", length(pvisible))
            if (1 == length(pvisible)) {
                message("i=", i, " visible")
                LON <- pvisible[[1]][,1]
                LAT <- pvisible[[1]][,2]
                mapPolygon(LON, LAT, col=if (i %in% iii) 2 else 4)
            } else {
                message("i=", i, " not visible")
            }
        }
    }
    mapLines(eowLL$longitude, eowLL$latitude, col="forestgreen")
 }
