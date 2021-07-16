library(oce)
library(rgdal)
source("~/git/oce/R/map.R")
doplot <- !FALSE
gridTest <- !FALSE

debug <- 0L
nlowroot <- 16                         #  0.09 km (= 6371/2^16)
nlowroot <- 15                         #  0.19 km (= 6371/2^15)
nlowroot <- 14                         #  0.39 km (= 6371/2^14)
nlowroot <- 13                         #  0.79 km (= 6371/2^13)
nlowroot <- 12                         #  1.5  km (= 6371/2^12)
nlowroot <- 11                         #  3.1  km (= 6371/2^11)
nlowroot <- 10                         #  6.2  km (= 6371/2^10)
#nlowroot <- 9                          # 12    km (= 6371/2^9)
#nlowroot <- 8                          # 25    km (= 6371/2^8)
#nlowroot <- 7                          # 50    km (= 6371/2^7)
#nlowroot <- 12 # 1.5 km # eow03_20210714T074152.pdf
#nlowroot <- 10 # 6.2 km
ntheta <- 360
ntheta <- 180
ntheta <- 180
ntheta <- 90

if (!interactive()) pdf("eow02.pdf")

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
projs <- c("ortho +lon_0=-30 +lat_0=-20",
           "robin",
           "moll")
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
    ll <- try(rgdal::project(cbind(x,y), proj, inv=TRUE), silent=TRUE)
    if (inherits(ll, "try-error") || !is.finite(ll[[1,1]])) -1 else 1
}


for (proj in projs) {
    proj <- paste0("+proj=", proj)
    message(proj)
    mapPlot(coastlineWorld, proj=proj, col="black", type=if(doplot) "l" else "n")
    mtext(proj)
    usr <- par("usr")
    x0 <- mean(usr[1:2])
    y0 <- mean(usr[3:4])
    R <- vector("numeric", ntheta) # FIXME: for testing
    Rmax <- (1/2) * sqrt((usr[2]-usr[1])^2 + (usr[4]-usr[3])^2) # hypotenuese
    thetas <- seq(0.0, 2*pi, length.out=ntheta)
    owarn <- options()$warn
    options(warn=-1)
    # TIMING.  I think nlowroot=10, ntheta=90 is a reasonable choice.
    # user-time nlowroot ntheta Notes
    #      1.09       10     90   6 km resolution
    #      1.05        9     90  12 km resolution (insignificant speedup)
    #      2.08       10    180   6 km resolution (time scales as ntheta)
    print(system.time({
    for (i in seq_along(thetas)) {
        capture.output({
            lr <- lowroot(func, xlow=0, xhigh=Rmax, n=nlowroot, theta=thetas[i])
        })
        R[i] <- lr$x
        if (!FALSE&&doplot)
            lines(x0+c(0,R[i]*cos(thetas[i])), y0+c(0,R[i]*sin(thetas[i])), col=4)
    }
    }))
    X <- x0 + R * cos(thetas)
    Y <- y0 + R * sin(thetas)
    options(warn=owarn)
    ok <- is.finite(X) & is.finite(Y)
    if (any(!ok))
        warning("got ", sum(!ok), " non-finite values, i.e. ", round(100*sum(!ok)/length(ok),2), "% of total")
    if (doplot) {
        lines(X, Y, col="red")
    }
    # Earth polygon (ends mismatch by 1e-8 for ortho)
    message(sprintf("x spans %.0fkm to %.0fkm",
                    min(X,na.rm=TRUE)/1e3, max(X,na.rm=TRUE)/1e3))
    message(sprintf("y spans %.0fkm to %.0fkm",
                    min(Y,na.rm=TRUE)/1e3, max(Y,na.rm=TRUE)/1e3))
    X[length(X)] <- X[1]
    Y[length(Y)] <- Y[1]
    eow <- sf::st_polygon(list(outer=cbind(X, Y)))
    projBase <- gsub(".*=", "", gsub(" .*$", "", proj))
    projBase <- paste0("eow_", projBase , ".rda")
    save(eow, file=projBase)
    message("saved to ", projBase)
    clon <- coastlineWorld[["longitude"]]
    clat <- coastlineWorld[["latitude"]]
}
