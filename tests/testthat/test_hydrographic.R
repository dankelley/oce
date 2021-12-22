# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Test whether base-level [[ can handle hydrographic computations

library(oce)
data(ctd)
S <- ctd[["salinity"]]
T <- ctd[["temperature"]]
p <- ctd[["pressure"]]
lon <- ctd[["longitude"]]
lat <- ctd[["latitude"]]
# The two theta values differ by max 4.819825e-6 degC
thetaUNESCO <- swTheta(S, T, p, eos="unesco")
thetaGSW <- swTheta(S, T, p, longitude=lon, latitude=lat, eos="gsw")
CT <- ctd[["CT"]]                      # GSW
SA <- ctd[["SA"]]                      # GSW
Sstar <- ctd[["Sstar"]]                # GSW

test_that("base-level [[\"?\"]] works on hydrographic things (UNESCO)", {
    options(oceEOS="unesco")
    n <- new("oce")
    n <- oceSetData(n, "salinity", S)
    n <- oceSetData(n, "temperature", T)
    n <- oceSetData(n, "pressure", p)
    expect_equal(thetaUNESCO, n[["theta"]])
    expect_error(n[["CT"]], "object lacks location information")
    expect_error(n[["SA"]], "object lacks location information")
    expect_error(n[["Sstar"]], "object lacks location information")
})

test_that("base-level [[\"?\"]] works on hydrographic things (GSW)", {
    options(oceEOS="gsw")
    n <- new("oce")
    n <- oceSetData(n, "salinity", S)
    n <- oceSetData(n, "temperature", T)
    n <- oceSetData(n, "pressure", p)
    n <- oceSetMetadata(n, "longitude", lon)
    n <- oceSetMetadata(n, "latitude", lat)
    expect_equal(thetaGSW, n[["theta"]])
    expect_equal(CT, n[["CT"]])
    expect_equal(SA, n[["SA"]])
    expect_equal(Sstar, n[["Sstar"]])
})

