rm(list=ls())
# eow02sf_02.R patterned on eow02sf.R but set +f=0 to get ortho inverse
library(oce)
fill <- TRUE                           # to fill land polygons in LH panel
drawRadii <- FALSE
drawCircumferance <- TRUE

#source("~/git/oce/R/map.R")
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
gridTest <- !FALSE

debug <- 0L
nlowroot <- 16                         #  0.09 km (= 6371/2^16)
nlowroot <- 15                         #  0.19 km (= 6371/2^15)
nlowroot <- 14                         #  0.39 km (= 6371/2^14)
nlowroot <- 13                         #  0.79 km (= 6371/2^13)
nlowroot <- 12                         #  1.5  km (= 6371/2^12)
nlowroot <- 11                         #  3.1  km (= 6371/2^11)
nlowroot <- 10                         #  6.2  km (= 6371/2^10)
nlowroot <- 10                         # 12    km (= 6371/2^9)
#nlowroot <- 12 # 1.5 km # eow03_20210714T074152.pdf
#nlowroot <- 10 # 6.2 km
ntheta <- 360 # 4.3  s with nlowroot=15
ntheta <- 180 # 2.5  s with nlowroot=15
ntheta <- 180 # 1.8  s with nlowroot=12
ntheta <- 90  # 1.0  s with nlowroot=12
ntheta <- 45
ntheta <- 20
# Time ntheta nlowroot Notes
#  4.3    360       15
#  2.5    180       15
#  1.8    180       12
#  1.1     90       15
#  0.91    90       12
#  2.7     90       16
#  1.4     90       14 eow03.pdf looks good, acceptably slow

if (!interactive()) pdf("eow02sf.pdf")

#' assume decreasing function.
#' @param f a function that decreases as x increases
#' @param xlow an x value that yields f>0
#' @param xhigh a larger x value
#' @param n number of iterations. Note that 3*6371e3/2^15=583.2825 so dx typically under 1km
#' @param ... extra parameters, passed to f()
#' @return highest x that has f(x) > 0
lowroot <- function(f, xlow, xhigh, n=15L, ...)
{
    flow <- f(xlow, ...)
    if (flow <= 0.0)
        stop("f(xlow) must be positive, but it is ", flow)
    fhigh <- f(xhigh, ...)
    if (fhigh > 0.0)
        return(fhigh) # +ve at xhigh
    while (n > 0)
    {
        xmid <- 0.5 * (xlow + xhigh)
        fmid <- f(xmid, ...)
        if (fmid < 0.0) {
            xhigh <- xmid
        } else if (fmid > 0.0) {
            xlow <- xmid
        } else {
            break
        }
        n <- n - 1L
    }
    list(x=xlow, dx=xhigh-xlow)
}

FAC <- 0.99
data(coastlineWorld)
projs <- c("+proj=ortho +lon_0=-30 +lat_0=-20", "+proj=robin", "+proj=moll")
proj0 <- "+proj=longlat +datum=WGS84 +no_defs +R=6378137 +f=0"
projs <- projs[1] # testing
nprojs <- length(projs)
par(mar=c(3,2,2,1))
par(mfrow=c(nprojs, 3))

func <- function(r, theta, proj, proj0="+proj=longlat +datum=WGS84 +no_defs +R=6378137 +f=0")
{
    #message("in func, proj=", proj)
    x <- x0 + r*cos(theta)
    y <- y0 + r*sin(theta)
    t <- try(sf::sf_project(proj, proj0, cbind(x, y), warn=FALSE), silent=TRUE)
    if (inherits(t, "try-error")) -1 else 1
}

for (proj in projs) {
    proj <- projFix(proj)
    message(proj)
    if (fill)
        mapPlot(coastlineWorld, proj=proj, col="lightgray")
    else 
       mapPlot(coastlineWorld, proj=proj, type=if(doplot) "l" else "n")
    axis(1)
    axis(2)
    mtext(proj, cex=par("cex"))
    mtext("eow02sf_02.R", line=1, cex=par("cex"))
    usr <- par("usr")
    x0 <- mean(usr[1:2])
    y0 <- mean(usr[3:4])
    R <- X <- Y <- vector("numeric", ntheta) # FIXME: for testing
    Rmax <- (1/2) * sqrt((usr[2]-usr[1])^2 + (usr[4]-usr[3])^2) # hypotenuese
    thetas <- seq(0.0, 2*pi, length.out=ntheta)
    for (i in seq_along(thetas)) {
        #message("i=",i, ", theta=", thetas[i], ", proj=", proj, ", proj0=", proj0)
        lr <- lowroot(func, xlow=0, xhigh=Rmax, n=nlowroot, theta=thetas[i], proj=proj, proj0=proj0)
        R[i] <- lr$x
        if (debug>1L)
            message(sprintf("i=%d, theta=%6.4f, f(%.0f+-%.0f)=%.0f (Rmax=%.0f)",
                            i, thetas[i], R[i], lr$dx, func(R[i], thetas[i]), Rmax))
        X[i] <- x0 + R[i] * cos(thetas[i])
        Y[i] <- y0 + R[i] * sin(thetas[i])
        if (drawRadii)
            lines(x0+c(0,R[i]*cos(thetas[i])), y0+c(0,R[i]*sin(thetas[i])), col=4)
    }
    if (drawCircumferance)
        lines(X, Y, col="red")
    LL <- map2lonlat(X, Y)
    DF <- data.frame(theta=thetas,
                     lon=LL$longitude, lat=LL$latitude,
                     X=X, Y=Y)
    print(DF[1:6,], digits=3)
    if (!all(is.finite(DF$lon) & is.finite(DF$lat))) {
        warning("edge-of-world encountered some non-finite values")
        ok <- is.finite(DF$lon) & is.finite(DF$lat)
        DF <- df[ok,]
    }
    if (drawCircumferance)
        mapLines(DF$lon, DF$lat, col="forestgreen", lty="dotted")
    # Earth polygon (ends mismatch by 1e-8 for ortho)
    XX <- X
    YY <- Y
    XX[length(XX)] <- XX[1]
    YY[length(YY)] <- YY[1]
    eow <- sf::st_polygon(list(outer=cbind(XX, YY)))
    projBase <- gsub(".*=", "", gsub(" .*$", "", proj))
    projBase <- paste0("eow_", projBase , ".rda")
    save(eow, file=projBase)
    message("saved to ", projBase)
}

# Middle panel: coastline plot, to check eowLL
plot(coastlineWorld)

#> # a point on AA, on "back side", from locator(1)
#> AAXY <- cbind(107596.6, -6133536)
#> AALL <- sf::sf_project(proj, proj0, AAXY, warn=FALSE)
#> AALL
#>points(AALL[1,1], AALL[1,2], col=2, pch=20)

eowLL <- sf::sf_project(proj, proj0, eow, keep=TRUE, warn=!FALSE)
if (grepl("=ortho", proj)) { # ortho is tricky: add lines to whichever pole is visible
    NPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,90), keep=TRUE, warn=FALSE))
    SPoleVisible <- !anyNA(sf::sf_project(proj0, proj, cbind(0,-90), keep=TRUE, warn=FALSE))
    lonOrder <- order(eowLL[,1])
    eowLL <- eowLL[lonOrder,]
    #t <- function(){plot(eowLL[,1],eowLL[,2]);polygon(eowLL[,1],eowLL[,2],col='pink');abline(h=-90);abline(v=c(-180,180))}
    if (SPoleVisible) {
        neow <- dim(eowLL)[1]
        eowLL <- rbind(c(-180, eowLL[1L, 2L]), eowLL)
        eowLL <- rbind(c(-180, -90), eowLL)
        eowLL <- rbind(eowLL, c(180, eowLL[neow+2L,2L]))
        eowLL <- rbind(eowLL, c(180, -90))
    }
    if (NPoleVisible) {
        neow <- dim(eowLL)[1]
        eowLL <- rbind(c(-180, eowLL[1L, 2L]), eowLL)
        eowLL <- rbind(c(-180, 90), eowLL)
        eowLL <- rbind(eowLL, c(180, eowLL[neow+2L,2L]))
        eowLL <- rbind(eowLL, c(180, 90))
    }
    # close the polygon
    eowLL <- rbind(eowLL, c(eowLL[1,]))
}
points(eowLL[,1], eowLL[,2], col=2)
polygon(eowLL[,1], eowLL[,2], border="blue", col=rgb(1,0,0,0.1))
mtext("pink shows visible polygon (extended to pole)", cex=par("cex"))

# Right panel: mapPlot again
mapPlot(coastlineWorld, proj=proj, type="n")


load("coastlineWorldPolygon.rda")

eowLLpoly <- sf::st_polygon(list(eowLL))

for (i in seq_along(coastlineWorldPolygon)) {
    #i <- 218
    cll <- coastlineWorldPolygon[[i]][[1]]
    cpoly <-sf::st_polygon(list(cll))
    vcpoly <- sf::st_intersection(cpoly, eowLLpoly)
    if (length(vcpoly) > 0L) {
        #message("i=", i, " is visible")
        if (inherits(vcpoly, "MULTIPOLYGON")) {
            for (ipoly in seq_len(length(vcpoly))) { # how to find n polys?
                if (debug>1L)
                    message("multipolygon at i=", i, ", ipoly=", ipoly)
                P <- sf::sf_project(proj0, proj, vcpoly[[ipoly]][[1]], keep=TRUE, warn=FALSE)
                polygon(P, col="tan")
            }
        } else {
            P <- sf::sf_project(proj0, proj, vcpoly[[1]], keep=TRUE, warn=FALSE)
            polygon(P, col="tan")
        }
    }
}
mtext("tan shows trimmed nation outlines", cex=par("cex"))
mtext("the Zimbabwe hole is under investigation", cex=par("cex"), side=1)

