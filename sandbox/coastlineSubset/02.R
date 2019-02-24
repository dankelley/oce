library(oce)
library(sp)
debug <- !TRUE
##span0 <- 8*geodDist(-180,-90,0,0)
##z <- 1
if (!interactive()) pdf("02.pdf")
par(mfrow=c(1, 1))
## 1. Q: can we recognize points in polygon?
##> data(coastlineWorldFine, package="ocedata")
##> cl <- coastlineWorldFine
data(coastlineWorld)
cl <- coastlineWorld
lon <- cl[["longitude"]]
lat <- cl[["latitude"]]
eps <- 1e-3
E <- -35
E <- 134
W <- -70
N <- 52
N <- -30
S <- 35
px <- c(-70, -70, -45, -45)
py <- c(35, 52, 52, 35)

##.n <- length(lon)

na <- which(is.na(lon))
nseg <- length(na)

##examine <- 118
cllon <- cl[["longitude"]]
cllat <- cl[["latitude"]]
par(mar=c(2, 2, 1, 1))
while (TRUE) {
    plot(cl)# , clon=E, clat=N, span=span0/z, type="l", pch=20)
    ##plot(cl)
    abline(v=E, col=4)
    ##abline(h=N, col=4)
    mtext(sprintf("E=%.1f N=%.1f (click to revise); close window to exit",
                  E, N), side=3, col=2, font=2, cex=0.8)
    cat("--reset--\n")
    for (iseg in 2:nseg) {
        look <- seq.int(na[iseg-1]+1, na[iseg]-1)
        lon <- cllon[look]
        if (any(is.na(lon))) stop("double lon NA at seg=", iseg)
        lat <- cllat[look]
        if (any(is.na(lat))) stop("double lat NA at seg=", iseg)
        n <- length(lon)
        LAT <- rep(NA_real_, n)
        LON <- rep(NA_real_, n)
        j <- 1L
        for (i in seq.int(1, n)) {
            if (lon[i] < E) {
                if (i>1 && lon[i-1] >=E) {
                    ## Interpolate crossing point
                    cat("seg=", iseg, ": case 1: last outside (i=", i, " j=", j, ")\n")
                    latfake <- if (lon[i] == lon[i-1])
                        0.5*(lat[i-1] + lat[i])
                    else
                        lat[i-1] + (lat[i] - lat[i-1])/(lon[i]-lon[i-1])*(E-lon[i-1])
                    LAT[j] <- latfake
                    LON[j] <- E
                    if (debug > 0) {
                        points(E, latfake, col="red", pch="+")
                        points(lon[i-1], lat[i-1], col="blue", pch="+", cex=.9)
                        points(lon[i], lat[i], col="darkgreen", pch="+", cex=.9)
                    }
                    j <- j + 1
                }
                LAT[j] <- lat[i]
                LON[j] <- lon[i]
                j <- j + 1
            } else if (lon[i] >= E) {
                if (i > 1 && lon[i-1] < E) {
                    cat("seg=", iseg, ": case 2: last was outside (i=", i, " j=", j, ")\n")
                    latfake <- if (lon[i] == lon[i-1])
                        0.5*(lat[i-1] + lat[i])
                    else
                        lat[i-1] + (lat[i] - lat[i-1])/(lon[i]-lon[i-1])*(E-lon[i-1])
                    LAT[j] <- latfake
                    LON[j] <- E
                    if (debug > 0) {
                        points(LON[j], LAT[j], col="blue", pch=2)
                    }
                    j <- j + 1
                }
            }
        }
        polygon(LON, LAT, col=rgb(1,0,0,alpha=0.2))
        ## if (debug && iseg == examine) {
        ##     plot(lon,lat,asp=1/cos(pi*mean(lat)/180), type="o",pch=20,cex=1/2)
        ##     polygon(lon, lat, col=rgb(0,1,0,alpha=0.1))
        ##     polygon(LON, LAT, col=rgb(1,0,0,alpha=0.2))
        ##     points(lon, lat, pch=20, col=2) # data
        ##     points(lon[1], lat[1], pch=1, col=2) # data
        ##     points(LON, LAT, pch=20, col=3, cex=1/2) # why an extra point?
        ##     browser()
        ## }
    }
    ##> polygon(LON, LAT, col=rgb(1,0,0,alpha=0.2))
    if (!interactive())
        break
    yusr <- par("usr")[3:4]
    high <- yusr[1] + 0.95*(yusr[2] - yusr[1])
    ##cat("high=", high, "\n")
    #abline(h=high, lwd=3)
    xy <- locator(1)
    #if (xy$y > high) {
    #    break
    #}
    E <- xy$x
    N <- xy$y
    ##span <- span / 1.1
    ##if (debug) {
    ##    m <- menu(c("world view", "focus", "+ zoom in", "- zoom out", "quit"), "action")
    ##    if (m == 1) {z <- 1; E <- 0; N <- 0}
    ##    else if (m == 2) z <- 1
    ##    else if (m == 3) z <- z * 1.1
    ##    else if (m == 4) z <- z / 1.1
    ##    else if (m == 5) break
    ##}
}
##LAT <- LAT[1:j]
##LON <- LON[1:j]

if (!interactive()) dev.off()
