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

trimIsopycnal <- function(S, T, longitude, latitude, eos="unesco")
{
    # Find intersection point of data and FPL
    FPL <- if (eos == "unesco") {
        function(S) swTFreeze(S, 0, eos=eos)
    } else {
        function(S) swTFreeze(S, 0, longitude=longitude, latitude=latitude, eos=eos)
    }
    m <- approxfun(S, T, rule=2)
    frozen <- T < FPL(S)
    if (any(frozen)) {
        if (all(frozen)) {
            return(list(S=NULL, T=NULL))
        }
        u <- uniroot(function(S) {m(S) - FPL(S)}, range(S, na.rm=TRUE))
        Skeep <- S[!frozen]
        Tkeep <- T[!frozen]
        return(list(S=c(u$root, Skeep), T=c(FPL(u$root), Tkeep)))
    }
    list(S=S, T=T)
}

# DEVELOPER TESTS:
# 1. set `eos <- "gsw"`
# 2. add 2 to T (which then will not have sub-freezing data)
# 3. subtract 20 from T (which then will not have non-freezing data)

eos <- "unesco" # also try "gsw"
longitude <- -63 # only used for eos="gsw"
latitude <- 40 # only used for eos="gsw"

# Create fake data (obviously not an actual isopycnal!)
S <- seq(30, 34, length.out=20)
T <- -3 + (S-30) + (S-30)^2 # add 2 to make it all warmer than FPL
plot(S, T, type="l")
# Show FPL
FPL <- if (eos == "unesco") {
    function(S) swTFreeze(S, 0, eos=eos)
} else {
    function(S) swTFreeze(S, 0, longitude=longitude, latitude=latitude, eos=eos)
}
Sf <- seq(min(S), max(S), length.out=100)
lines(Sf, FPL(Sf), col=4)

ST <- trimIsopycnal(S, T)
lines(ST$S, ST$T, lwd=3)
legend("topleft", lwd=c(1, 3, 1), col=c(1, 1, 4),
    legend=c("cold data", "warm data", "FPL"))

