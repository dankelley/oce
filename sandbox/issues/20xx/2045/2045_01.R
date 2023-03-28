# sandbox/issues/20xx/2045/2045_1.R
#
# Illustrate the discrepancy between unesco and gsw density formulations.
# Frankly, I am a bit surprised by the magnitude of the difference.
#
# See https://github.com/dankelley/oce/issues/2045

library(oce)
S <- 30
T <- 14
p <- 0
lon <- -63
lat <- 43
CTD <- as.ctd(S, T, p, longitude=lon, latitude=lat)
rhoU <- swRho(CTD, eos="unesco")[1]
rhoG <- swRho(CTD, eos="gsw")[1]

# Before proceeding to a unesco/gsw comparison, ensure that swRho() yields
# the same results for either a ctd object or S, T etc values.
stopifnot(identical(rhoU, swRho(S, T, p, eos="unesco")))
stopifnot(identical(rhoG, swRho(S, T, p, longitude=lon, latitude=lat, eos="gsw")))

# Demonstrate that GSW is the same if called without oce.  (I think this is
# already tested in the oce test suite but may as well show it here also.)
SA <- gsw_SA_from_SP(S, p, lon, lat)
CT <- gsw_CT_from_t(SA, T, p)
stopifnot(identical(rhoG, gsw_rho(SA, CT, p)))

# Compare unesco and gsw
rhoU - rhoG

# See if there are variations with location (yes)
CTD2 <- as.ctd(S, T, p, longitude=lon+10, latitude=lat)
rhoU2 <- swRho(CTD2, eos="unesco")[1]
rhoG2 <- swRho(CTD2, eos="gsw")[1]
rhoU2 - rhoG2
