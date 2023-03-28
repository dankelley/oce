# sandbox/issues/20xx/2046/2046_02.R
#
# Extend 2046_02.R in terms of a function.  This is a proof
# of concept for trimming isopycnals below FPL (freezing point
# line), extending 2046_02.R by phrasing the work in a
# function. The curve is not density, but just a function
# that crosses the FPL
#
# see https://github.com/dankelley/oce/issues/2046
library(oce)

trimIsopycnalLine <- function(contourline, longitude, latitude, eos="unesco")
{
    # See sandbox/issues/20xx/2046/2046_0[1:3].R, noting that here
    # x is S and y is T. The method centres on finding the
    # intersection, if any, between y=y(x) and the freezing point
    # line, FPL.
    x <- contourline$x
    y <- contourline$y
    FPL <- if (eos == "unesco") {
        function(x) swTFreeze(x, 0, eos=eos)
    } else {
        function(x) swTFreeze(x, 0, longitude=longitude, latitude=latitude, eos=eos)
    }
    frozen <- y < FPL(x)
    if (any(frozen)) {
        if (all(frozen)) {
            contourline$x <- NULL
            contourline$y <- NULL
        }
        # Bracket the crossing points and then find intersection of that line
        # with the FPL.
        after <- which(!frozen)[1]
        if (after < 2L) {
            # something is wrong. Just give up on the cutoff at the FPL
            return(contourline)
        }
        before <- after - 1L
        m <- approxfun(x[c(before, after)], y[c(before, after)], rule=2)
        # In an earlier attempt, m() approximated the whole curve.  But that
        # made for a failure in the root-finder if the salinity was so low that
        # the isopycnal crossed an isohaline line at two points.  (This happened
        # in the test of lobo plotting, since that machine was in very fresh
        # water.)
        u <- try(uniroot(function(x) {m(x) - FPL(x)}, range(x[c(before, after)], na.rm=TRUE)), silent=TRUE)
        if (!inherits(u, "try-error")) {
            xkeep <- x[!frozen]
            ykeep <- y[!frozen]
            contourline$x <- c(u$root, xkeep)
            contourline$y <- c(FPL(u$root), ykeep)
        }
    }
    contourline
}

# DEVELOPER TESTS:
# 1. set `eos <- "gsw"`
# 2. add 2 to T (which then will not have sub-freezing data)
# 3. subtract 20 from T (which then will not have non-freezing data)

eos <- "unesco" # also try "gsw"
longitude <- -63 # only used for eos="gsw"
latitude <- 40 # only used for eos="gsw"

# Create fake data (obviously not an actual isopycnal!)
S <- seq(30, 32, length.out=20)
T <- -3 + (S-30) + runif(1, 0.0, 0.8)*(S-30)^2 # add 2 to make it all warmer than FPL
plot(S, T, type="l")
# Show FPL
FPL <- if (eos == "unesco") {
    function(S) swTFreeze(S, 0, eos=eos)
} else {
    function(S) swTFreeze(S, 0, longitude=longitude, latitude=latitude, eos=eos)
}
Sf <- seq(min(S), max(S), length.out=100)
lines(Sf, FPL(Sf), col=4)
cl <- list(value=1, x=S, y=T)
clTrimmed <- trimIsopycnalLine(cl, longitude, latitude, eos)
lines(clTrimmed$x, clTrimmed$y, lwd=3)
legend("topleft", lwd=c(1, 3, 1), col=c(1, 1, 4),
    legend=c("cold data", "warm data", "FPL"))

