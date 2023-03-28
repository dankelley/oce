# sandbox/issues/20xx/2044/2044_01.R
#
# Test an idea for having plotTS() draw isopycnals by plotting. I had this idea
# whilst working on issue 2044, but it was not needed there.  Still, the code is
# worth keeping, in case the idea proves useful at some later time.
#
# see https://github.com/dankelley/oce/issues/2044
library(oce)
# Fake data
SAd <- c(34.83376, 34.84024)
CTd <- c(0.86380, 0.89620) # for standalone

SAd <- c(30, 35)
CTd <- c(-3, 5) # for standalone


plot(SAd, CTd)
# Grid
NSA <- 70
NCT <- 50
usr <- par("usr")
SA <- seq(usr[1], usr[2], length.out=NSA)
CT <- seq(usr[3], usr[4], length.out=NCT)
grid <- expand.grid(CT=CT, SA=SA, KEEP.OUT.ATTRS=FALSE)
sigma0 <- gsw_sigma0(SA=grid$SA, CT=grid$CT)
levels <- pretty(sigma0, n=10)
sigma0 <- matrix(sigma0, byrow=TRUE, nrow=NSA, ncol=NCT)
contour(SA, CT, sigma0, levels=levels, add=TRUE, labcex=1)
# Freezing-point curve
CTf <- gsw_CT_freezing(SA, 0.0, saturation_fraction=1)
lines(SA, CTf, col=4)

cl <- contourLines(SA, CT, sigma0, levels=levels)
for (cc in cl) {
    lines(cc$x, cc$y, col=2, lty=2, lwd=2)
    points(tail(cc$x,1), tail(cc$y,1), pch=20, col=4)
    hitTop <- abs(tail(cc$y,1) - usr[4]) < (usr[4] - usr[3]) / (2*NCT)
    if (hitTop) {
        mtext(cc$level, side=3, at=tail(cc$x,1))
    } else {
        mtext(cc$level, side=4, at=tail(cc$y,1))
    }
}
