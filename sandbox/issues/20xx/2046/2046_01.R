# sandbox/issues/20xx/2046/2046_01.R
#
# Proof of concept for trimming isopycnals below FPL (freezing point line)
# The curve is not density, but just a function that crosses the FPL
#
# see https://github.com/dankelley/oce/issues/2046
library(oce)

eos <- "unesco" # also try "gsw"
longitude <- -63 # only used for eos="gsw"
latitude <- 40 # only used for eos="gsw"

# Create fake data (obviously not an actual isopycnal!)
S <- seq(30, 34, length.out=20)
T <- -3 + (S-30) + (S-30)^2 # add 2 to make it all warmer than FPL
plot(S, T, type="l")

# Add freezing point line
FPL <- if (eos == "unesco") {
    function(S) swTFreeze(S, 0, eos=eos)
} else {
    function(S) swTFreeze(S, 0, longitude=longitude, latitude=latitude, eos=eos)
}
Sf <- seq(min(S), max(S), length.out=100) # 100 is OK since nearly linear
lines(Sf, FPL(Sf), col=4)

# Find intersection point of data and FPL
m <- approxfun(S, T, rule=2)
frozen <- T < FPL(S)
if (any(frozen)) {
    u <- uniroot(function(S) {m(S) - FPL(S)}, range(S, na.rm=TRUE))
    # points(u$root, m(u$root))
    Skeep <- S[!frozen]
    Tkeep <- T[!frozen]
    Skeep <- c(u$root, Skeep)
    Tkeep <- c(FPL(u$root), Tkeep)
    #points(Skeep, Tkeep)
    lines(Skeep, Tkeep, lwd=3)
} else {
    lines(S, T, lwd=3)
}
legend("topleft", lwd=c(1, 3, 1), col=c(1, 1, 4),
    legend=c("cold data", "warm data", "FPL"))
