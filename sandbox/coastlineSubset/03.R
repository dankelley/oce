library(oce)
library(sp)
debug <- !TRUE
par(mfrow=c(1, 1))
##> data(coastlineWorldFine, package="ocedata")
##> cl <- coastlineWorldFine
data(coastlineWorld)
cl <- coastlineWorld
lon <- cl[["longitude"]]
lat <- cl[["latitude"]]


cllon <- cl[["longitude"]]             # gets revised at each of 4 steps
cllat <- cl[["latitude"]]              # "


par(mfrow=c(3, 2), mar=c(2, 2, 2, 1))

plot(cl)# , clon=E, clat=N, span=span0/z, type="l", pch=20)
mtext("Click southwest corner, then northest corner", cex=2/3)
SW <- locator(1)
W <- SW$x
S <- SW$y
NE <- locator(1)
E <- NE$x
N <- NE$y
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="cyan", lwd=1)

na <- which(is.na(cllon))
nseg <- length(na)
message("STEP 1 trim to west of E (starting with ", nseg, " segments)")
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
cllon <- lonTrimmed
cllat <- latTrimmed

plot(cl)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="cyan", lwd=1)
polygon(cllon, cllat, col="pink")
mtext(sprintf("after step 1 (trim to west of E=%.2f)", E), cex=2/3)

na <- which(is.na(cllon))
nseg <- length(na)
message("STEP 2 trim to east of W (starting with ", nseg, " segments)")
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
cllon <- lonTrimmed
cllat <- latTrimmed

plot(cl)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="cyan", lwd=1)
polygon(cllon, cllat, col="pink")
mtext(sprintf("after step 2 (trim to east of W=%.2f)", W), cex=2/3)

na <- which(is.na(cllon))
nseg <- length(na)
message("STEP 3 trim to south of N (starting with ", nseg, " segments)")
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
        } else if (lat[i] == N) {
            ## Point is within the edge
            LON[j] <- lon[i]
            LAT[j] <- lat[i]
            j <- j + 1
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
            }
        }
    }
    if (length(LON) > 2) {
        lonTrimmed <- c(lonTrimmed, NA, LON)
        latTrimmed <- c(latTrimmed, NA, LAT)
    }
}
cllon <- lonTrimmed
cllat <- latTrimmed

plot(cl)
polygon(cllon, cllat, col="pink")
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="cyan", lwd=1)
mtext(sprintf("after step 3 (trim to south of N=%.2f)", N), cex=2/3)


na <- which(is.na(cllon))
nseg <- length(na)
message("STEP 4 trim to north of S (starting with ", nseg, " segments)")
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
cllon <- lonTrimmed
cllat <- latTrimmed

plot(cl)
lines(c(W, W, E, E, W), c(S, N, N, S, S), col="cyan", lwd=1)
polygon(cllon, cllat, col="pink")
mtext(sprintf("after step 4 (trim to north of S=%.2f)", S), cex=2/3)

res <- as.coastline(cllon, cllat)
plot(res)
mtext("FINAL", side=1, cex=2/3)

##polygon(cllon, cllat, col="pink")
