library(oce)
library(sp)
debug <- !TRUE
usePanels <- !FALSE
repeatValues <- !TRUE # repeat initial values as clicked (for debugging only!)

par(mfrow=c(1, 1))
##> data(coastlineWorldFine, package="ocedata")
##> cl <- coastlineWorldFine
data(coastlineWorld)
cl <- coastlineWorld

## we add NA at start and end because that's how we find segments.
NAendpoints <- function(x) {
    if (!is.na(x[1]))
        x <- c(NA, x)
    if (!is.na(x[length(x)]))
        x <- c(x, NA)
    x
}
cllon <- NAendpoints(cl[["longitude"]]) # gets revised at each of 4 steps
cllat <- NAendpoints(cl[["latitude"]]) # "


par(mar=c(2, 2, 2, 1))
if (usePanels)
    par(mfrow=c(3, 2))

##{{{
plot(cl)# , clon=E, clat=N, span=span0/z, type="l", pch=20)
if (!repeatValues || !exists("S")) {
    mtext("Click southwest corner, then northest corner", cex=2/3)
    SW <- locator(1)
    W <- SW$x
    S <- SW$y
    NE <- locator(1)
    E <- NE$x
    N <- NE$y
}

lines(c(W, W, E, E, W), c(S, N, N, S, S), col="red", lwd=1)
##}}}

##{{{
na <- which(is.na(cllon))
nseg <- length(na)
message("RIGHT: trim to west of E (starting with ", nseg, " segments)")
lonTrimmed <- latTrimmed <- NULL
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    lon <- cllon[look]
    if (any(is.na(lon))) stop("step 1: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
    lat <- cllat[look]
    if (any(is.na(lat))) stop("step 1: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
    n <- length(lon)
    if (n < 1) stop("how can we have no data?")
    LON <- LAT <- NULL # so we can accumulate and not need to trim NA later
    j <- 1L
    for (i in seq.int(1L, n)) {
        if (lon[i] < E) {
            ## point is inside, so retain it, possibly after adding a fake point in the edge
            if (i > 1L && lon[i-1L] > E) {
                latfake <- if (lon[i] == lon[i-1L])
                    0.5*(lat[i-1L] + lat[i])
                else
                    lat[i-1L] + (lat[i]-lat[i-1L])/(lon[i]-lon[i-1L])*(E-lon[i-1L])
                LAT[j] <- latfake
                LON[j] <- E
                j <- j + 1L
            }
            LAT[j] <- lat[i]
            LON[j] <- lon[i]
            j <- j + 1L
        } else if (lon[i] == E) {
            ## Point is within the edge
            LON[j] <- lon[i]
            LAT[j] <- lat[i]
            j <- j + 1
        } else {
            ## Point is outside, but if the previous was inside, we insert a
            ## fake point within the edge.
            if (i > 1L && lon[i-1L] < E) {
                latfake <- if (lon[i] == lon[i-1L])
                    0.5*(lat[i-1L] + lat[i])
                else
                    lat[i-1L] + (lat[i]-lat[i-1L])/(lon[i]-lon[i-1L]) * (E-lon[i-1L])
                LAT[j] <- latfake
                LON[j] <- E
                j <- j + 1L
            }
        }
    }
    if (length(LON) > 2) {
        lonTrimmed <- c(lonTrimmed, NA, LON)
        latTrimmed <- c(latTrimmed, NA, LAT)
    }
}
cllon <- NAendpoints(lonTrimmed)
cllat <- NAendpoints(latTrimmed)
plot(cl)
polygon(cllon, cllat, col=2:5)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="red", lwd=1)
mtext(sprintf("after RIGHT (trim to west of E=%.2f)", E), cex=2/3)
##}}}

##{{{
na <- which(is.na(cllon))
nseg <- length(na)
message("BOTTOM: trim to north of S (starting with ", nseg, " segments)")
lonTrimmed <- latTrimmed <- NULL
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    lon <- cllon[look]
    if (any(is.na(lon))) stop("step 4: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
    lat <- cllat[look]
    if (any(is.na(lat))) stop("step 4: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
    n <- length(lon)
    if (n < 1) stop("how can we have no data?")
    LON <- LAT <- NULL # so we can accumulate and not need to trim NA later
    j <- 1L
    for (i in seq.int(1L, n)) {
        if (lat[i] > S) {
            ## point is inside, so retain it, possibly after adding a fake point in the edge
            if (i > 1L && lat[i-1L] < S) {
                lonfake <- if (lat[i] == lat[i-1L])
                    0.5*(lon[i-1L] + lon[i])
                else
                    lon[i-1L] + (lon[i]-lon[i-1L])/(lat[i]-lat[i-1L]) * (S-lat[i-1L])
                LAT[j] <- S
                LON[j] <- lonfake
                j <- j + 1L
            }
            LAT[j] <- lat[i]
            LON[j] <- lon[i]
            j <- j + 1L
        } else if (lat[i] == S) {
            ## Point is within the edge
            LON[j] <- lon[i]
            LAT[j] <- lat[i]
            j <- j + 1
        } else {
            ## Point is outside, but if the previous was inside, we insert a
            ## fake point within the edge.
            if (i > 1L && lat[i-1L] > S) {
                lonfake <- if (lat[i] == lat[i-1L])
                    0.5*(lon[i-1L] + lon[i])
                else
                    lon[i-1L] + (lon[i]-lon[i-1L])/(lat[i]-lat[i-1L]) * (S-lat[i-1L])
                LAT[j] <- S
                LON[j] <- lonfake
                j <- j + 1L
            }
        }
    }
    if (length(LON) > 2) {
        lonTrimmed <- c(lonTrimmed, NA, LON)
        latTrimmed <- c(latTrimmed, NA, LAT)
    }
}
cllon <- NAendpoints(lonTrimmed)
cllat <- NAendpoints(latTrimmed)
plot(cl)
polygon(cllon, cllat, col=2:5)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="red", lwd=1)
mtext(sprintf("after BOTTOM (trim to north of S=%.2f)", S), cex=2/3)
##}}}

##{{{
na <- which(is.na(cllon))
nseg <- length(na)
message("LEFT: trim to east of W (starting with ", nseg, " segments)")
lonTrimmed <- latTrimmed <- NULL
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    lon <- cllon[look]
    if (any(is.na(lon))) stop("step 2: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
    lat <- cllat[look]
    if (any(is.na(lat))) stop("step 2: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
    n <- length(lon)
    if (n < 1) stop("how can we have no data?")
    LON <- LAT <- NULL # so we can accumulate and not need to trim NA later
    j <- 1L
    for (i in seq.int(1L, n)) {
        if (lon[i] > W) {
            ## point is inside, so retain it, possibly after adding a fake point in the edge
            if (i > 1L && lon[i-1L] < W) {
                latfake <- if (lon[i] == lon[i-1L])
                    0.5*(lat[i-1L] + lat[i])
                else
                    lat[i-1L] + (lat[i]-lat[i-1L])/(lon[i]-lon[i-1L]) * (W-lon[i-1L])
                LAT[j] <- latfake
                LON[j] <- W
                j <- j + 1L
            }
            LAT[j] <- lat[i]
            LON[j] <- lon[i]
            j <- j + 1L
        } else if (lon[i] == W) {
            ## Point is within the edge
            LON[j] <- lon[i]
            LAT[j] <- lat[i]
            j <- j + 1
        } else {
            ## Point is outside, but if the previous was inside, we insert a
            ## fake point within the edge.
            if (i > 1L && lon[i-1L] > W) {
                latfake <- if (lon[i] == lon[i-1L])
                    0.5*(lat[i-1L] + lat[i])
                else
                    lat[i-1L] + (lat[i]-lat[i-1L])/(lon[i]-lon[i-1L]) * (W-lon[i-1L])
                LAT[j] <- latfake
                LON[j] <- W
                j <- j + 1L
            }
        }
    }
    if (length(LON) > 2) {
        lonTrimmed <- c(lonTrimmed, NA, LON)
        latTrimmed <- c(latTrimmed, NA, LAT)
    }
}
cllon <- NAendpoints(lonTrimmed)
cllat <- NAendpoints(latTrimmed)
plot(cl)
polygon(cllon, cllat, col=2:5)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="red", lwd=1)
mtext(sprintf("after LEFT (trim to east of W=%.2f)", W), cex=2/3)
##}}}


##{{{
na <- which(is.na(cllon))
nseg <- length(na)
message("TOP: trim to south of N (starting with ", nseg, " segments)")
lonTrimmed <- latTrimmed <- NULL
for (iseg in 2:nseg) {
    look <- seq.int(na[iseg-1]+1, na[iseg]-1)
    lon <- cllon[look]
    if (any(is.na(lon))) stop("step 3: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
    lat <- cllat[look]
    if (any(is.na(lat))) stop("step 3: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
    n <- length(lon)
    if (n < 1) stop("how can we have no data?")
    LON <- LAT <- NULL # so we can accumulate and not need to trim NA later
    j <- 1L
    lastWasFake <- NULL
    for (i in seq.int(1L, n)) {
        if (lat[i] < N) {
            ## point is inside, so retain it, possibly after adding a fake point in the edge
            if (i > 1L && lat[i-1L] > N) {
                lonfake <- if (lat[i] == lat[i-1L])
                    0.5*(lon[i-1L] + lon[i])
                else
                    lon[i-1L] + (lon[i]-lon[i-1L])/(lat[i]-lat[i-1L]) * (N-lat[i-1L])
                LAT[j] <- N
                LON[j] <- lonfake
                j <- j + 1L
            }
            LAT[j] <- lat[i]
            LON[j] <- lon[i]
            j <- j + 1L
            lastWasFake <- FALSE
        } else if (lat[i] == N) {
            ## Point is within the edge
            LON[j] <- lon[i]
            LAT[j] <- lat[i]
            j <- j + 1
            lastWasFake <- FALSE
        } else {
            ## Point is outside, but if the previous was inside, we insert a
            ## fake point within the edge.
            if (i > 1L && lat[i-1L] < N) {
                lonfake <- if (lat[i] == lat[i-1L])
                    0.5*(lon[i-1L] + lon[i])
                else
                    lon[i-1L] + (lon[i]-lon[i-1L])/(lat[i]-lat[i-1L]) * (N-lat[i-1L])
                LAT[j] <- N
                LON[j] <- lonfake
                j <- j + 1L
                lastWasFake <- TRUE
            }
        }
    }
    if (length(LON) > 2) {
        ##> message("iseg=", iseg, " j-1=", j-1, " LON=", LON[j-1], " LAT=", LAT[j-1], " W=", W, " lastWasFake=", lastWasFake)
        message("range(LON)=", paste(range(LON), collapse=" "), " (W=", W, ", E=", E, ")")
        if (LON[1] == W) {
            NWpip <- sp::point.in.polygon(W, N, cl[["longitude"]], cl[["latitude"]])
            if (NWpip) {
                message("adding initial NW vertex at N=", N, " W=", W)
                LON <- c(W, LON)
                LAT <- c(N, LAT)
            }
        } else if (LON[j-1] == W) {
            NWpip <- sp::point.in.polygon(W, N, cl[["longitude"]], cl[["latitude"]])
            if (NWpip) {
                message("adding final NW vertex at N=", N, " W=", W)
                LON <- c(LON, W)
                LAT <- c(LAT, N)
            }
        } else if (LON[1] == E) {
            NEpip <- sp::point.in.polygon(E, N, cl[["longitude"]], cl[["latitude"]])
            if (NEpip) {
                message("adding initial NE vertex at N=", N, " E=", E)
                LON <- c(E, LON)
                LAT <- c(N, LAT)
            }
        } else if (LON[j-1] == E) {
            NEpip <- sp::point.in.polygon(E, N, cl[["longitude"]], cl[["latitude"]])
            if (NEpip) {
                message("adding final NE vertex at N=", N, " E=", E)
                LON <- c(LON, E)
                LAT <- c(LAT, N)
            }
        }
        lonTrimmed <- c(lonTrimmed, NA, LON)
        latTrimmed <- c(latTrimmed, NA, LAT)
    }
}
cllon <- NAendpoints(lonTrimmed)
cllat <- NAendpoints(latTrimmed)
plot(cl)
polygon(cllon, cllat, col=2:5)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="red", lwd=1)
mtext(sprintf("after TOP (trim to south of N=%.2f)", N), cex=2/3)
##}}}


##{{{
res <- as.coastline(cllon, cllat)
plot(res, col=2:5)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="red", lwd=1)
fac <- length(cl[["longitude"]]) / length(cllon)
mtext(sprintf("FINAL: size reduced by factor %.2f", fac), cex=2/3)
##}}}

##polygon(cllon, cllat, col="pink")
