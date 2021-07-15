options(oceEOW=TRUE)

# iii is trying to see why S Amer is messed up
#iii <- c(6,7,9,11,15,34,35,36,72,73,80,81,90,92,102,104,112,113,119,120,122,146,185,197,198,211,215,238,244,249,261,269,281)
iii <- c(6,7,34,35,36,72,73,80,81,90,92,102,104,119,120,122,146,185,197,198,211,215,249,261,269,281)
if (!interactive()) pdf("eow03.pdf", width=7, height=4, pointsize=9)
par(mar=rep(1,4))
library(oce)
#source("~/git/oce/R/map.R")
library(sf)
data(coastlineWorld)
projs <- c("ortho +lon_0=-30 +lat_0=-20",
           "robin",
           "moll")
for (proj in projs) {
    message(proj)
    load(paste0("eow_", gsub(" .*$", "", proj) , ".rda"))
    lon <- coastlineWorld[["longitude"]]
    lat <- coastlineWorld[["latitude"]]
    mapPlot(lon, lat, proj=paste0("+proj=",proj), col="lightgray")
    LL <- eow[[1]]
    eowLL <- oce::map2lonlat(LL[,1], LL[,2])
    mapLines(eowLL$longitude, eowLL$latitude, col=4)
    eow2 <- sf::st_polygon(list(cbind(eowLL$longitude, eowLL$latitude)))

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
        if (i %in% iii) {
            plot(pvisible, add=TRUE, col="pink")
        }
    }
    lines(eowLL$longitude, eowLL$latitude, col=4)
    # the real test
    par(mar=rep(1, 4))

    mapPlot(coastlineWorld, proj=paste0("+proj=",proj), type="n")
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
                #if (i %in% iii) message("  single")
                LON <- pvisible[[1]][,1]
                LAT <- pvisible[[1]][,2]
                # iii[1,5] split badly (some in 1:4 also)
                # 1 tiny island
                # 3 just on edge (not visible to my eye)
                # 4 okay (Bolivia)
                # 5 badly split (Brazil). in points 71:74, jumps far south.
                mapPolygon(LON, LAT, col=if (i %in% iii) 2 else 4)
                #print(data.frame(LON,LAT))
                #DAN<-list(LON=LON,LAT=LAT);message("exported DAN at i=", i)
                }
            } else {
                for (L in seq_len(length(pvisible))) {
                    #if (i %in% iii) message("  multiple (L=", L, ")")
                    LON <- pvisible[[L]][[1]][,1]
                    LAT <- pvisible[[L]][[1]][,2]
                    mapPolygon(LON, LAT, col=if (i %in% iii) 2 else 4)
                }
            }
        }
        #plot(pvisible, add=TRUE, col="pink")
        #mapPolygon(lonPortion, latPortion, col=2, lwd=2)
    }
    mapLines(eowLL$longitude, eowLL$latitude, col="forestgreen")
 }
