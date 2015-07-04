library(oce)
source("~/src/oce/R/sw.R")
# Table of contents.
#  1. rho and sigma
#  2. potential temperature
#  3. Absolute Salinity and Conservative Temperature
#  4. sound speed
#  5. freezing temperature
#  6. specific heat
#  7. adiabatic lapse rate
#  8. alpha and beta
#  9. swSTrho
# 10. sound absorption (unesco only)
# 11. viscosity (unesco only)
# 12. thermal conductivity (unesco only)
# 13. electrical conductivity
# 14. depth and pressure [FIXME: INCOMPLETE]
# 15. spiciness

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
# [2] Trevor J. McDougall, 1987. Neutral Surfaces, Journal of Physical
#     Oceanography, volume 17, pages 1950-1964.

library(oce)

# 1. rho and sigma
# 1.1 UNESCO rho [1 p19]
S <- c( 0,   0,   0,   0,  35,  35,  35,  35)
T <- T90fromT68(c( 5,   5,  25,  25,   5,   5,  25,  25))
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

warning("swTheta() etc not tested!\n")
if (FALSE){
# 2 potential temperature
# 2.1 UNESCO potential temperature
#
# The following is an official test value from [1 p44], first with all args,
# second with a ctd object as an arg.
stopifnot(all.equal(swTheta(40, T90fromT68(40), 10000, eos="unesco"), 36.89073, scale=1, tolerance=0.00002))
stopifnot(all.equal(swTheta(as.ctd(40, T90fromT68(40), 10000), eos="unesco"), 36.89073, scale=1, tolerance=0.00002))

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
theta <- swTheta(ctd, eos="gsw")
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
# 8.1 UNESCO alpha and beta
# The formulae used are not actually from UNESCO, but are rather given in [2],
# and the test values come from [2] also.
#
# Since [2] gives formula is in terms of theta=10C, we must compute the
# corresponding in-situ temperature first. Use S=40 and 4000dbar to match
# his check value.
T <- uniroot(function(x) 10-swTheta(40, x, 4000, eos="unesco"), c(9, 12))$root
# The beta=7.2088e-4 value is from the last sentence of McDougall's Appendix.
stopifnot(all.equal.numeric(7.2088e-4, swBeta(40, T, 4000, eos="unesco"), scale=1, tolerance=1e-8))
# The alpha/beta=0.34763 is from the left-hand column of McDougall's p1964.
stopifnot(all.equal.numeric(0.34763, swAlphaOverBeta(40, T, 4000, eos="unesco"), scale=1, tolerance=1e-5))
stopifnot(all.equal.numeric(0.34763*7.20883e-4, swAlpha(40, T, 4000, eos="unesco"), scale=1, tolerance=1e-5))
# 8.1 GSW alpha and beta
# Check against gsw_ values, which we know to be correct from the gsw test suite.
SP <- 40
t <- 10
p <- 4000
lon <- 300
lat <- 30
a <- swAlpha(SP, t, p, longitude=lon, latitude=lat, eos="gsw")
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


# 10. sound absorption
# Compared with Table IV of Fisher & Simmons 1977.
alpha <- swSoundAbsorption(100e3, 35, 4, 4990) # at 500 atm (4990 dbar of water)
stopifnot(all.equal.numeric(alpha, 0.0175, tolerance=0.01)) # 1% test
alpha <- swSoundAbsorption(10e3, 35, 4, 0) # expect 0.00083 at 1 atm (0dbar of water)
stopifnot(all.equal.numeric(alpha, 0.000829, tolerance=0.01)) # 1% test


# 11. viscosity
visc <- swViscosity(30, 10)
stopifnot(all.equal.numeric(visc, 0.001383779, tolerance=1e-7))


# 12. thermal conductivity
# No check values available, but investigation shows agreement
# with Caldwell's Table 1 to within the 0.5 percent tolerance
# he mentions.
cond <- swThermalConductivity(35, 10, 100)
stopifnot(all.equal.numeric(cond, 0.5822402))


# 13. electrical conductivity
stopifnot(all.equal.numeric(swCSTp(35,   15,   0, eos="unesco"), 1.000000))
stopifnot(all.equal.numeric(swCSTp(35,   15,   0, eos="gsw"),    1.000000))
stopifnot(all.equal.numeric(swSCTp(1.2, 20,2000, eos="unesco"), 37.245628, tolerance=1e-6))
stopifnot(all.equal.numeric(swSCTp(0.65, 5,1500, eos="unesco"), 27.995347, tolerance=1e-6))
stopifnot(all.equal.numeric(swSCTp(1,   15,   0, eos="unesco"), 35.000000, tolerance=1e-6))
stopifnot(all.equal.numeric(swSCTp(1.2, 20,2000, eos="unesco"), 37.245628, tolerance=1e-6))
stopifnot(all.equal.numeric(swSCTp(0.65, 5,1500, eos="unesco"), 27.995347, tolerance=1e-6))
data(ctd)
## This does not have conductivity, so add it
salinity <- ctd[["salinity"]]
temperature <- ctd[["temperature"]]
pressure <- ctd[["pressure"]]
conductivity <- swCSTp(salinity, temperature, pressure, eos="unesco")
ctd <- ctdAddColumn(ctd, conductivity, "conductivity")
S <- swSCTp(ctd)
misfit <- sqrt(mean((S-salinity)^2))
stopifnot(misfit < 1e-3)
# Test that swCSTp() takes both salinity and CTD [issue 630]
cond1 <- swCSTp(salinity, temperature, pressure, eos="unesco")
cond2 <- swCSTp(ctd)
stopifnot(all.equal.numeric(cond1, cond2))

## the C=1 value can be tested directly in gsw, but others are tested against gsw.
stopifnot(all.equal.numeric(swSCTp(1,   15,   0, eos="gsw"), 35.000000, tolerance=1e-6))
SP <- swSCTp(1.2, 20, 2000, eos="gsw")
stopifnot(all.equal.numeric(1.2, gsw_C_from_SP(SP, 20, 2000) / gsw_C_from_SP(35, 15, 0)))
stopifnot(all.equal.numeric(1.2, swCSTp(SP, 20, 2000, eos="gsw")))
SP <- swSCTp(0.65, 5, 1500, eos="gsw")
stopifnot(all.equal.numeric(0.65, gsw_C_from_SP(SP, 5, 1500) / gsw_C_from_SP(35, 15, 0)))
stopifnot(all.equal.numeric(0.65, swCSTp(SP, 5, 1500, eos="gsw")))


# 14. depth and pressure
# The UNESCO test is basically for consistency with old versions, I think, 
# but the GSW test is against gsw_z_from_p(), which is well-tested in
# the building of the gsw package.
depth <- swDepth(10000, 30, eos="unesco")
stopifnot(all.equal.numeric(depth, 9712.653, scale=1, tolerance=0.001))
depth <- swDepth(10000, 30, eos="gsw")
stopifnot(all.equal.numeric(depth, 9713.735, scale=1, tolerance=0.001))
pressure <- swPressure(9712.653, 30, eos="unesco")
stopifnot(all.equal.numeric(pressure, 10000., scale=1, tolerance=0.001))
pressure <- swPressure(9712.653, 30, eos="gsw")
stopifnot(all.equal.numeric(pressure, gsw_p_from_z(-9712.653, 30), scale=1, tolerance=0.001))
}

# 15. spiciness
sp <- swSpice(35, T90fromT68(10), 100)
stopifnot(all.equal.numeric(sp, 1.131195, tolerance=0.0000015))
