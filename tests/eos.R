# Tests of EOS (equation-of-state) and related seawater properties
library(oce)

lr <- sw.lapse.rate(40, 40, 10000)
stopifnot(all.equal.numeric(lr, 3.255976e-4, 1e-7))

cond <- sw.conductivity(35,10,100)
stopifnot(all.equal.numeric(cond, 0.618569, 1e-5))

visc <- sw.viscosity(30,10)
stopifnot(all.equal.numeric(visc, 0.001383779, 1e-7))

ctd <- as.ctd(35, 13, 1000)
stopifnot(all.equal.numeric(sw.rho(35, 13, 1000), 1030.818, 1e-6))
stopifnot(all.equal.numeric(sw.rho(ctd),          1030.818, 1e-6))

stopifnot(all.equal.numeric(sw.sigma(35, 13, 1000), 30.818, 1e-5))
stopifnot(all.equal.numeric(sw.sigma(ctd),          30.818, 1e-5))

stopifnot(all.equal.numeric(sw.theta(35, 13, 1000), 12.858, 1e-3))
stopifnot(all.equal.numeric(sw.theta(ctd),          12.858, 1e-3))

# Test values from page 9 of
# Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
# fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
# Science}, \bold{44}, 53 pp

stopifnot(all.equal.numeric(sw.S.C.T.p(1,   15,   0), 35.000000, 1e-6))
stopifnot(all.equal.numeric(sw.S.C.T.p(1.2, 20,2000), 37.245628, 1e-6))
stopifnot(all.equal.numeric(sw.S.C.T.p(0.65, 5,1500), 27.995347, 1e-6))

#S <- sw.S.T.rho(10, 22, 0)
#stopifnot(all.equal.numeric(S, 28.651, 1e-3))

C.p <- sw.specific.heat(40, 40, 10000)
stopifnot(all.equal.numeric(C.p, 3849.500, 1e-3))

TT <- sw.T.freeze(40, 500)
stopifnot(all.equal.numeric(TT, -2.588567, 1e-6))

ctd <- as.ctd(40, 10, 4000)
stopifnot(all.equal.numeric(sw.theta(40,10,4000,0,"unesco"), 9.42648, 1e-5))
stopifnot(all.equal.numeric(sw.theta(ctd,0,"unesco"), 9.42648, 1e-5))

ab <- sw.alpha.over.beta(40, 10, 4000, is.theta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, 0.00005))
ab <- sw.alpha.over.beta(ctd, is.theta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, 0.00005))

b <- sw.beta(40, 10, 4000, is.theta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))
b <- sw.beta(ctd, is.theta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))

a <- sw.alpha(40, 10, 4000, is.theta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))
a <- sw.alpha(ctd, is.theta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))

v <- sw.sound.speed(40, 40, 10000)
stopifnot(all.equal.numeric(v, 1731.995, 0.001))
v <- sw.sound.speed(as.ctd(40,40,10000))
stopifnot(all.equal.numeric(v, 1731.995, 0.001))

## spice (not from any trusted source, merely from the code [2008-10-02]
sp <- sw.spice(35,10,100)
stopifnot(all.equal.numeric(sp, 1.131195, 0.0000015))

## spice (not from any trusted source, merely from the code [2008-10-02]
depth <- sw.depth(10000, 30)
stopifnot(all.equal.numeric(depth, 9712.653, 0.001))

