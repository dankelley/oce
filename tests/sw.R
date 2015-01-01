# Table of contents.
# 1. rho and sigma
# 2. potential temperature
# 3. Absolute Salinity and Conservative Temperature
# 4. sound speed
# 5. freezing temperature
# 6. specific heat
# 7. adiabatic lapse rate
# 8. alpha and beta
# 9. swSTrho

## spec vol anom and dens anom
## pressure to depth

# The UNESCO properties are generally tested against UNESCO documents [1].
# The GSW properties are more complicated because its check values are generally
# stated in terms of Absolute Salinity (SA), not Practical Salinity (SP).
# However, we already know that the GSW functions are very well tested
# through tests of the 'gsw' package, so all we really need is a single
# test case for each function, to ensure that eos="gsw" is interpreted
# correctly, etc.
#
# References:
# [1] N. P. Fofonoff and R. C. Millard Jr., 1983.
#     Algorithms for computation of fundamental properties of seawater.
#     UNESCO technical papers in marine science, vol 44.
#     http://www.jodc.go.jp/info/ioc_doc/UNESCO_tech/059832eb.pdf

library(oce)

# 1. rho and sigma
# 1.1 UNESCO rho [1 p19]
S <- c( 0,   0,   0,   0,  35,  35,  35,  35)
T <- c( 5,   5,  25,  25,   5,   5,  25,  25)
p <- c( 0, 1e4,   0, 1e4,   0, 1e4,   0, 1e4)
rho <- c(999.96675, 1044.12802, 997.04796, 1037.90204, 1027.67547, 1069.48914, 1023.34306, 1062.53817)
stopifnot(all.equal.numeric(swRho(S, T, p, eos="unesco"), rho))
# check sigma from this
stopifnot(all.equal.numeric(swRho(S, T, p, eos="unesco")-1000, swSigma(S,T,p,eos="unesco")))
# 1.2 GSW
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


# 2 potential temperature
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
theta <- swTheta(SP, t, p, eos="gsw")
stopifnot(all.equal.numeric(thetaGSW, theta))
theta <- swTheta(ctd)
stopifnot(all.equal.numeric(thetaGSW, theta))

# 3.  Absolute Salinity and Conservative Temperature
CT <- gsw_CT_from_t(SA=SA, t=t, p=p)
stopifnot(all.equal.numeric(SA, swAbsoluteSalinity(salinity=SP, pressure=p, longitude=lon, latitude=lat)))
stopifnot(all.equal.numeric(SA, swAbsoluteSalinity(ctd)))
stopifnot(all.equal.numeric(CT, swConservativeTemperature(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat)))
stopifnot(all.equal.numeric(CT, swConservativeTemperature(ctd)))

# 4. sound speed 
# 4.1 UNESCO
stopifnot(all.equal.numeric(1731.995, swSoundSpeed(40, 40, 1e4, eos="unesco"), tolerance=0.001))
stopifnot(all.equal.numeric(1731.995, swSoundSpeed(as.ctd(40, 40, 1e4), eos="unesco"), tolerance=0.001))
SA <- gsw_SA_from_SP(SP=40, p=1e4, longitude=300, latitude=30)
CT <- gsw_CT_from_t(SA, 40, 1e4)
# 4.2 GSW sound speed
speedGSW <- gsw_sound_speed(SA, CT, 1e4)
speed <- swSoundSpeed(salinity=40, temperature=40, pressure=1e4, longitude=300, latitude=30, eos="gsw")
stopifnot(all.equal.numeric(speedGSW, speed))

# 5. Freezing temperature
# 5.1 UNESCO freezing temperature [1 p29]
Tf <- swTFreeze(40, 500, eos="unesco")
stopifnot(all.equal.numeric(Tf, -2.588567, tolerance=1e-6))
# 5.2 GSW freezing temperature 
SA <- gsw_SA_from_SP(SP=40, p=500, longitude=300, latitude=30)
TfGSW <- gsw_t_freezing(SA=SA, p=500, saturation_fraction=1)
Tf <- swTFreeze(40, 500, longitude=300, latitude=30, eos="gsw")
stopifnot(all.equal.numeric(TfGSW,Tf))

# 6. specific heat
# 6.1 UNESCO specific heat [1 p31]
p <- 1e4
t <- 40
SP <- 40
lon <- 300
lat <- 30
C <- swSpecificHeat(salinity=SP, temperature=t, pressure=p, eos="unesco")
stopifnot(all.equal.numeric(C, 3849.500, tolerance=1e-3))
# 6.2 GSW specific heat
SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
CGSW <- gsw_cp_t_exact(SA=SA, t=t, p=p)
C <- swSpecificHeat(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat, eos="gsw")
stopifnot(all.equal.numeric(CGSW, C))

# 7. Adiabatic lapse rate
# 7.1 UNESCO lapse rate [1 p38]
SP <- 40
t <- 40
p <- 1e4
l <- swLapseRate(salinity=SP, temperature=t, pressure=p, eos="unesco")
stopifnot(all.equal.numeric(l, 3.255976e-4, tolerance=1e-7))
# 7.2 GSW lapse rate
SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
CT <- gsw_CT_from_t(SA=SA, t=t, p=p)
lGSW <- 1e4*gsw_adiabatic_lapse_rate_from_CT(SA=SA, CT=CT, p=p) # convert to deg/m
l <- swLapseRate(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat, eos="gsw")
stopifnot(all.equal.numeric(lGSW, l))

# 8 alpha and beta
# 8.1 UNESCO alpha and beta (tested internally only) FIXME: add more authorative tests
SP <- 40
t <- 10
p <- 4000
lon <- 300
lat <- 30
a <- swAlpha(salinity=SP, temperature=t, pressure=p, eos="unesco")
stopifnot(all.equal.numeric(a, 0.0002470394481351, 1e-7, scale=1e-4))
b <- swBeta(salinity=SP, temperature=t, pressure=p, eos="unesco")
stopifnot(all.equal.numeric(b, 0.0007217063743196, 1e-7, scale=1e-4))
ctd <- as.ctd(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat)
a <- swAlpha(ctd, eos="unesco")
stopifnot(all.equal.numeric(a, 0.0002470394481351, 1e-7, scale=1e-4))
b <- swBeta(ctd, eos="unesco")
stopifnot(all.equal.numeric(b, 0.0007217064, 0.0005, scale=1e-4))
# 8.2 GSW alpha and beta
## FIXME: alpha here
b <- swBeta(SP, t, p, longitude=lon, latitude=lat, eos="gsw")
SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
CT <- gsw_CT_from_t(SA=SA, t=t, p=p)
aGSW <- gsw_alpha(SA=SA, CT=CT, p=p)
stopifnot(all.equal.numeric(a, aGSW))
bGSW <- gsw_beta(SA=SA, CT=CT, p=p)
stopifnot(all.equal.numeric(b, bGSW))

# 9. swSTrho
# This is used to draw isopycnals on TS diagrams.
t <- 10
rho <- 1022
# 9.1 UNESCO swSTrho
Su <- swSTrho(t, rho, 0, eos="unesco")
stopifnot(all.equal(Su, 28.65114808083))
stopifnot(all.equal(rho, swRho(Su, t, 0, eos="unesco")))

# 9.2 GSW swSTrho
Sg <- swSTrho(t, rho, 0, eos="gsw")
stopifnot(all.equal(Sg, 28.76287326771))
stopifnot(all.equal.numeric(rho, gsw_rho(Sg, t, 0)))


# MISC FUNCTIONS

# sound absorption, compared with Table IV of Fisher & Simmons 1977.
alpha <- swSoundAbsorption(100e3, 35, 4, 4990) # at 500 atm (4990 dbar of water)
stopifnot(all.equal.numeric(alpha, 0.0175, tolerance=0.01)) # 1% test
alpha <- swSoundAbsorption(10e3, 35, 4, 0) # expect 0.00083 at 1 atm (0dbar of water)
stopifnot(all.equal.numeric(alpha, 0.000829, tolerance=0.01)) # 1% test


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



if (FALSE) {
    ctd2 <- ctd(40, 10, 4000, longitude=300, latitude=30)
    ab <- swAlphaOverBeta(40, 10, 4000)
    stopifnot(all.equal.numeric(ab, 0.34763, tolerance=0.00005))
    ab <- swAlphaOverBeta(ctd2)
    stopifnot(all.equal.numeric(ab, 0.34763, tolerance=0.00005))

    a <- swAlpha(40, 10, 4000)
    stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))
    a <- swAlpha(ctd2)
    stopifnot(all.equal.numeric(a, 2.5060e-4, 0.0005, scale=1e-4))

    ## spice (not from any trusted source, merely from the code [2008-10-02])
    sp <- swSpice(35,10,100)
    stopifnot(all.equal.numeric(sp, 1.131195, tolerance=0.0000015))
} else {
    message("FIXME: need tests on alpha, beta, spice but first must code!!")
}

depth <- swDepth(10000, 30)
stopifnot(all.equal.numeric(depth, 9712.653, tolerance=0.001))
pressure <- swPressure(9712.653, 30)
stopifnot(all.equal.numeric(pressure, 10000., tolerance=0.001))

