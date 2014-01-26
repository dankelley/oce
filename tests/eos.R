# Tests of EOS (equation-of-state) and related seawater properties
library(oce)

# sound absorption, compared with Table IV of Fisher & Simmons 1977.
alpha <- swSoundAbsorption(100e3, 35, 4, 4990) # at 500 atm (4990 dbar of water)
stopifnot(all.equal.numeric(alpha, 0.0175, tolerance=0.01)) # 1% test
alpha <- swSoundAbsorption(10e3, 35, 4, 0) # expect 0.00083 at 1 atm (0dbar of water)
stopifnot(all.equal.numeric(alpha, 0.000829, tolerance=0.01)) # 1% test

lr <- swLapseRate(40, 40, 10000)
stopifnot(all.equal.numeric(lr, 3.255976e-4, tolerance=1e-7))

cond <- swConductivity(35,10,100)
stopifnot(all.equal.numeric(cond, 0.618569, tolerance=1e-5))

visc <- swViscosity(30,10)
stopifnot(all.equal.numeric(visc, 0.001383779, tolerance=1e-7))

ctd <- as.ctd(35, 13, 1000)
stopifnot(all.equal.numeric(swRho(35, 13, 1000), 1030.818, tolerance=1e-6))
stopifnot(all.equal.numeric(swRho(ctd),          1030.818, tolerance=1e-6))

stopifnot(all.equal.numeric(swSigma(35, 13, 1000), 30.818, tolerance=1e-5))
stopifnot(all.equal.numeric(swSigma(ctd),          30.818, tolerance=1e-5))

stopifnot(all.equal.numeric(swTheta(35, 13, 1000), 12.858, tolerance=1e-3))
stopifnot(all.equal.numeric(swTheta(ctd),          12.858, tolerance=1e-3))

# Test values from page 9 of
# Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
# fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
# Science}, \bold{44}, 53 pp

stopifnot(all.equal.numeric(swSCTp(1,   15,   0), 35.000000, tolerance=1e-6))
stopifnot(all.equal.numeric(swSCTp(1.2, 20,2000), 37.245628, tolerance=1e-6))
stopifnot(all.equal.numeric(swSCTp(0.65, 5,1500), 27.995347, tolerance=1e-6))

#S <- swSTrho(10, 22, 0)
#stopifnot(all.equal.numeric(S, 28.651, 1e-3))

Cp <- swSpecificHeat(40, 40, 10000)
stopifnot(all.equal.numeric(Cp, 3849.500, tolerance=1e-3))

TT <- swTFreeze(40, 500)
stopifnot(all.equal.numeric(TT, -2.588567, tolerance=1e-6))

ctd <- as.ctd(40, 10, 4000)
stopifnot(all.equal.numeric(swTheta(40,10,4000,0,"unesco"), 9.42648, tolerance=1e-5))
stopifnot(all.equal.numeric(swTheta(ctd,0,"unesco"), 9.42648, tolerance=1e-5))

ab <- swAlphaOverBeta(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, tolerance=.00005))
ab <- swAlphaOverBeta(ctd, isTheta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, tolerance=0.00005))

b <- swBeta(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))
b <- swBeta(ctd, isTheta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))

a <- swAlpha(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))
a <- swAlpha(ctd, isTheta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))

v <- swSoundSpeed(40, 40, 10000)
stopifnot(all.equal.numeric(v, 1731.995, tolerance=0.001))
v <- swSoundSpeed(as.ctd(40,40,10000))
stopifnot(all.equal.numeric(v, 1731.995, tolerance=0.001))

## spice (not from any trusted source, merely from the code [2008-10-02]
sp <- swSpice(35,10,100)
stopifnot(all.equal.numeric(sp, 1.131195, tolerance=0.0000015))

depth <- swDepth(10000, 30)
stopifnot(all.equal.numeric(depth, 9712.653, tolerance=0.001))
pressure <- swPressure(9712.653, 30)
stopifnot(all.equal.numeric(pressure, 10000., tolerance=0.001))

## TEOS-10
## check value from the TEOS-10 supplied program 'gsw_check_functions.c'
## CAUTION: It seems likely that the tests will fail, if TEOS-10 is altered
## or replaced; in such a case, the test values used below will be kept as it is 
## (since the committee has recommended use of TEOS-10) but the tests will be
## relaxed.
if (FALSE) {
    sp <- 35.5                         # practical salinity
    t <- 15                            # in-situ temperature
    p <- 300                           # pressure
    lon <- 260                         # longitude
    lat <- 16                          # latitude
    sa <- teos("gsw_sa_from_sp", sp, p, lon, lat)
    stopifnot(abs(35.671358392019094 - sa) < 1.3e-10)
    ct <- teos("gsw_ct_from_t", 35.7, t, p)
    stopifnot(abs(14.930280459895560 - ct) < 6.3e-10)
    rho <- teos("gsw_rho", 35.7, 20, p)
    stopifnot(abs(1026.4562376198473 - rho) < 2.9e-10)
    sa <- 35.7
    stopifnot(abs(1512.2053940303056 - teos("gsw_sound_speed_t_exact", sa, t, p)) < 2.6e-9)
    stopifnot(abs(212.30166821093002 - teos("gsw_entropy_t_exact", sa, t, p))     < 9.0e-9)
    stopifnot(abs(3982.7832563441461 - teos("gsw_cp_t_exact", sa, t, p))          < 2.8e-9)
}
