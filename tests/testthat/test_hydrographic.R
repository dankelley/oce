# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Test whether base-level [[ can handle hydrographic computations

library(oce)
data(ctd)

S <- ctd[["salinity"]]
T <- ctd[["temperature"]]
p <- ctd[["pressure"]]
lon <- ctd[["longitude"]]
lat <- ctd[["latitude"]]
nlevel <- length(S)

# Object with no location data (for UNESCO testing)
nu <- new("oce")
nu <- oceSetData(nu, "salinity", S)
nu <- oceSetData(nu, "temperature", T)
nu <- oceSetData(nu, "pressure", p)

# Object with location data (for GSW testing)
ng <- new("oce")
ng <- oceSetData(ng, "salinity", S)
ng <- oceSetData(ng, "temperature", T)
ng <- oceSetData(ng, "pressure", p)
ng <- oceSetMetadata(ng, "longitude", lon)
ng <- oceSetMetadata(ng, "latitude", lat)

# The two theta values differ by max 4.819825e-6 degC
thetaUNESCO <- swTheta(S, T, p, eos="unesco")
thetaGSW <- swTheta(S, T, p, longitude=lon, latitude=lat, eos="gsw")

test_that("base-level [[\"?\"]] gives expected names (UNESCO)", {
    # using sort() because the order (for some reason I do not understand) is
    # different when I do it interactively, versus in the test as set up by
    # RStudio.  I guess RStudio is using a different default for sorting
    # character values.
    expect_equal(sort(nu[["?"]]$dataDerived), sort(c("density", "depth", "N2",
                paste("potential", "temperature"), "Rrho", "RrhoSF",
                "sigmaTheta", paste("sound", "speed"), "SP", "spice", "theta",
                "z")))
})

test_that("base-level [[\"?\"]] works on some hydrographic things (UNESCO)", {
    options(oceEOS="unesco")
    expect_equal(ctd[["theta"]], nu[["theta"]])
    expect_error(nu[["CT"]], "need longitude and latitude to compute")
    expect_error(nu[["SA"]], "need longitude and latitude to compute")
    expect_error(nu[["Sstar"]], "need longitude and latitude to compute")
})


test_that("base-level [[\"?\"]] gives expected names (GSW)", {
    expect_equal(sort(ng[["?"]]$dataDerived), sort(c(paste("Absolute",
                    "Salinity"), paste("Conservative", "Temperature"), "CT",
                "density", "depth", "N2", paste("potential", "temperature"),
                "Rrho", "RrhoSF", "SA", "sigma0", "sigma1", "sigma2", "sigma3",
                "sigma4", "sigmaTheta", paste("sound", "speed"), "SP", "spice",
                "SR", "Sstar", "theta", "z")))
})

test_that("base-level [[\"?\"]] works on some hydrographic things (GSW)", {
    options(oceEOS="gsw")
    expect_equal(ctd[["theta"]], ng[["theta"]])
    expect_equal(ctd[["CT"]], ng[["CT"]])
    expect_equal(ctd[["SA"]], ng[["SA"]])
    expect_equal(ctd[["Sstar"]], ng[["Sstar"]])
})

test_that("base-level [[\"?\"]] all names work (UNESCO)", {
    options(oceEOS="unesco")
    a <- nu[["?"]]$dataDerived
    expect_true(all(nlevel == sapply(a, function(i) sum(is.finite(nu[[i]])))))
})

test_that("base-level [[\"?\"]] all names work (GSW)", {
    options(oceEOS="gsw")
    a <- ng[["?"]]$dataDerived
    expect_true(all(nlevel == sapply(a, function(i) sum(is.finite(ng[[i]])))))
})


