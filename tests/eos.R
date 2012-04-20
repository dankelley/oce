# Tests of EOS (equation-of-state) and related seawater properties
library(oce)

lr <- swLapseRate(40, 40, 10000)
stopifnot(all.equal.numeric(lr, 3.255976e-4, 1e-7))

cond <- swConductivity(35,10,100)
stopifnot(all.equal.numeric(cond, 0.618569, 1e-5))

visc <- swViscosity(30,10)
stopifnot(all.equal.numeric(visc, 0.001383779, 1e-7))

ctd <- as.ctd(35, 13, 1000)
stopifnot(all.equal.numeric(swRho(35, 13, 1000), 1030.818, 1e-6))
stopifnot(all.equal.numeric(swRho(ctd),          1030.818, 1e-6))

stopifnot(all.equal.numeric(swSigma(35, 13, 1000), 30.818, 1e-5))
stopifnot(all.equal.numeric(swSigma(ctd),          30.818, 1e-5))

stopifnot(all.equal.numeric(swTheta(35, 13, 1000), 12.858, 1e-3))
stopifnot(all.equal.numeric(swTheta(ctd),          12.858, 1e-3))

# Test values from page 9 of
# Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
# fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
# Science}, \bold{44}, 53 pp

stopifnot(all.equal.numeric(swSCTp(1,   15,   0), 35.000000, 1e-6))
stopifnot(all.equal.numeric(swSCTp(1.2, 20,2000), 37.245628, 1e-6))
stopifnot(all.equal.numeric(swSCTp(0.65, 5,1500), 27.995347, 1e-6))

#S <- swSTrho(10, 22, 0)
#stopifnot(all.equal.numeric(S, 28.651, 1e-3))

Cp <- swSpecificHeat(40, 40, 10000)
stopifnot(all.equal.numeric(Cp, 3849.500, 1e-3))

TT <- swTFreeze(40, 500)
stopifnot(all.equal.numeric(TT, -2.588567, 1e-6))

ctd <- as.ctd(40, 10, 4000)
stopifnot(all.equal.numeric(swTheta(40,10,4000,0,"unesco"), 9.42648, 1e-5))
stopifnot(all.equal.numeric(swTheta(ctd,0,"unesco"), 9.42648, 1e-5))

ab <- swAlphaOverBeta(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, 0.00005))
ab <- swAlphaOverBeta(ctd, isTheta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, 0.00005))

b <- swBeta(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))
b <- swBeta(ctd, isTheta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))

a <- swAlpha(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))
a <- swAlpha(ctd, isTheta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))

v <- swSoundSpeed(40, 40, 10000)
stopifnot(all.equal.numeric(v, 1731.995, 0.001))
v <- swSoundSpeed(as.ctd(40,40,10000))
stopifnot(all.equal.numeric(v, 1731.995, 0.001))

## spice (not from any trusted source, merely from the code [2008-10-02]
sp <- swSpice(35,10,100)
stopifnot(all.equal.numeric(sp, 1.131195, 0.0000015))

depth <- swDepth(10000, 30)
stopifnot(all.equal.numeric(depth, 9712.653, 0.001))

## TEOS-10
## check value from the TEOS-10 supplid program 'gsw_check_functions.c'
## CAUTION: the tests check on agreement in the second-last digit of the test
## values.  It seems likely that the tests will fail, if TEOS-10 is altered
## or replaced; in such a case, the test values used below will be kept as it is 
## (since the committee has recommended use of TEOS-10) but the tests will be
## relaxed.
if (FALSE) {
    sa <- teos("gsw_sa_from_sp", 35.5, 300, 260, 16)
    stopifnot(abs(35.671358392019094 - sa) < 00.000000000000010)
    ct <- teos("gsw_ct_from_t", 35.7, 15, 300)
    stopifnot(abs(14.930280459895560 - ct) < 00.000000000000010)
    rho <- teos("gsw_rho", 35.7, 20, 300)
    stopifnot(abs(1026.4562376198473 - rho) < 0000.0000000000010)
}
