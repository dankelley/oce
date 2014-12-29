# Tests of EOS (equation-of-state) and related seawater properties
# NB. the various gsw_ functions are thoroughly tested in the gsw package,
# so we need only a single check-point for each function, which essentially
# tests whether the eos="gsw" condition has been handled.
#
# References:
# [1] Algorithms for computation of fundamental properties of seawater.
#     UNESCO tech. papers in marine science, vol 44, online at e.g.
#     http://www.jodc.go.jp/info/ioc_doc/UNESCO_tech/059832eb.pdf

library(oce)

# PART 1. Density and related functions.

# 1.1 UNESCO
#
# Following are official test values from [1 p19]. Once we know rho is OK
# we can check sigma based on it.
S <- c( 0,   0,   0,   0,  35,  35,  35,  35)
T <- c( 5,   5,  25,  25,   5,   5,  25,  25)
p <- c( 0, 1e4,   0, 1e4,   0, 1e4,   0, 1e4)
rho <- c(999.96675, 1044.12802, 997.04796, 1037.90204, 1027.67547, 1069.48914, 1023.34306, 1062.53817)
stopifnot(all.equal.numeric(swRho(S, T, p, eos="unesco"), rho))
stopifnot(all.equal.numeric(swRho(S, T, p, eos="unesco")-1000, swSigma(S,T,p,eos="unesco")))

# 1.2 GSW
#
# Since gsw_ functions are tested in the gsw package, we just need a consistency check.
longitude <- 188
latitude <- 4
SP <- 35
t <- 10
p <- 1000
SA <- gsw_SA_from_SP(SP, p, longitude, latitude)
CT <- gsw_CT_from_t(SA, t, p)
# Test density.
rhoGSW <- gsw_rho(SA, CT, p)
rho <- swRho(SP, t, p, longitude, latitude, "gsw")
stopifnot(all.equal.numeric(rhoGSW, rho))
# Now use density to test sigma (not provided by gsw).
sigma <- swSigma(SP, t, p, longitude, latitude, "gsw")
stopifnot(all.equal.numeric(rhoGSW-1000, sigma))


# PART 2. potential temperature

# 2.1 UNESCO potential temperature
#
# The following is an official test value from [1 p44], first with all args,
# second with a ctd object as an arg.
stopifnot(all.equal(swTheta(40, 40, 10000, eos="unesco"), 36.89073, tolerance=0.00001))
stopifnot(all.equal(swTheta(as.ctd(40, 40, 10000), eos="unesco"), 36.89073, tolerance=0.00001))

# 2.2 GSW potential temperature
# 
# Since gsw_ functions are tested in the gsw package, we just need a consistency check.
SP <- 35
t <- 13                                 # notation in gsw_...() functions
p <- 1000
lon <- 300
lat <- 30
ctd <- as.ctd(SP, t, p, longitude=lon, latitude=lat)
SA <- gsw_SA_from_SP(SP, p, longitude=lon, latitude=lat)
thetaGSW <- gsw_pt_from_t(SA, t, p, p_ref=0)
theta <- swTheta(SP,t,p,eos="gsw")
stopifnot(all.equal.numeric(thetaGSW, theta))


## PART 3. misc functions

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

ctd2 <- as.ctd(40, 10, 4000)
stopifnot(all.equal.numeric(swTheta(40,10,4000,0,eos="unesco"), 9.42648, tolerance=1e-5))
stopifnot(all.equal.numeric(swTheta(ctd2,referencePressure=0,eos="unesco"), 9.42648, tolerance=1e-5))

ab <- swAlphaOverBeta(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, tolerance=0.00005))
ab <- swAlphaOverBeta(ctd2, isTheta=TRUE)
stopifnot(all.equal.numeric(ab, 0.34763, tolerance=0.00005))

b <- swBeta(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))
b <- swBeta(ctd2, isTheta=TRUE)
stopifnot(all.equal.numeric(b, 7.2088e-4, 0.0005, scale=1e-4))

a <- swAlpha(40, 10, 4000, isTheta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))
a <- swAlpha(ctd2, isTheta=TRUE)
stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))

v <- swSoundSpeed(40, 40, 10000)
stopifnot(all.equal.numeric(v, 1731.995, tolerance=0.001))
v <- swSoundSpeed(as.ctd(40,40,10000))
stopifnot(all.equal.numeric(v, 1731.995, tolerance=0.001))

## spice (not from any trusted source, merely from the code [2008-10-02])
sp <- swSpice(35,10,100)
stopifnot(all.equal.numeric(sp, 1.131195, tolerance=0.0000015))

depth <- swDepth(10000, 30)
stopifnot(all.equal.numeric(depth, 9712.653, tolerance=0.001))
pressure <- swPressure(9712.653, 30)
stopifnot(all.equal.numeric(pressure, 10000., tolerance=0.001))


