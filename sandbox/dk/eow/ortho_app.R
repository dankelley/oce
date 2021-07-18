rm(list=ls())
library(oce)
doplot <- !FALSE
gridTest <- !FALSE
lon0 <- -20
lat0 <- -30

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

debug <- 0L
nlowroot <- 16                         #  0.09 km (= 6371/2^16)
nlowroot <- 15                         #  0.19 km (= 6371/2^15)
nlowroot <- 14                         #  0.39 km (= 6371/2^14)
nlowroot <- 13                         #  0.79 km (= 6371/2^13)
nlowroot <- 12                         #  1.5  km (= 6371/2^12)
nlowroot <- 11                         #  3.1  km (= 6371/2^11)
nlowroot <- 10                         #  6.2  km (= 6371/2^10)
nlowroot <- 9                          # 12    km (= 6371/2^9)
#nlowroot <- 12 # 1.5 km # eow03_20210714T074152.pdf
#nlowroot <- 10 # 6.2 km
ntheta <- 360 # 4.3  s with nlowroot=15
ntheta <- 180 # 2.5  s with nlowroot=15
ntheta <- 180 # 1.8  s with nlowroot=12
ntheta <- 90  # 1.0  s with nlowroot=12
ntheta <- 90  # 0.83 s with nlowroot=10
ntheta <- 45
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
projs <- c("+proj=ortho +lon_0=-30 +lat_0=-20",
           "+proj=robin",
           "+proj=moll")
if (interactive())
    projs <- projs[1]
nprojs <- length(projs)
par(mar=c(1,1,2,1))
if (interactive())
    par(mfrow=c(1,nprojs))

func <- function(r, theta)         # +1 if (r,theta) is visible, -1 otherwise
{
    x <- x0 + r*cos(theta)
    y <- y0 + r*sin(theta)
    ifelse(is.na(map2lonlat(x, y)$longitude), -1, 1)
}

proj <- projFix(paste0("+proj=ortho +lon_0=", lon0, " +lat_0=", lat0))
message(proj)
mapPlot(coastlineWorld, proj=proj, col="black", type=if(doplot) "l" else "n")
mtext(proj)
usr <- par("usr")
x0 <- mean(usr[1:2])
y0 <- mean(usr[3:4])
R <- X <- Y <- vector("numeric", ntheta) # FIXME: for testing
Rmax <- (1/2) * sqrt((usr[2]-usr[1])^2 + (usr[4]-usr[3])^2) # hypotenuese
thetas <- seq(0.0, 2*pi, length.out=ntheta)
for (i in seq_along(thetas)) {
    lr <- lowroot(func, xlow=0, xhigh=Rmax, n=nlowroot, theta=thetas[i])
    R[i] <- lr$x
    if (debug>1L)
        message(sprintf("i=%d, theta=%6.4f, f(%.0f+-%.0f)=%.0f (Rmax=%.0f)",
                        i, thetas[i], R[i], lr$dx, func(R[i], thetas[i]), Rmax))
    X[i] <- x0 + R[i] * cos(thetas[i])
    Y[i] <- y0 + R[i] * sin(thetas[i])
}
LL <- map2lonlat(X, Y)
DF <- data.frame(theta=thetas,
                 lon=LL$longitude, lat=LL$latitude,
                 X=X, Y=Y)
print(DF[1:6,], digits=3)
ok <- is.finite(LL$longitude) & is.finite(LL$latitude)
LL$longitude <- LL$longitude[ok]
LL$latitude <- LL$latitude[ok]

mapLines(LL$longitude, LL$latitude, col="forestgreen")

# Earth polygon (ends mismatch by 1e-8 for ortho)
XX <- X
YY <- Y
XX[length(XX)] <- XX[1]
YY[length(YY)] <- YY[1]
eow <- sf::st_polygon(list(outer=cbind(XX, YY)))
projBase <- gsub(".*=", "", gsub(" .*$", "", proj))
projBase <- paste0("eow_", projBase , ".rda")
### save(eow, file=projBase)
### message("saved to ", projBase)





