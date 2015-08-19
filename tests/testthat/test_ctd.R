## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(ctd)
context("CTD")

test_that("as.ctd() with specified arguments", {
          ctd_ctd <- as.ctd(salinity=ctd[["salinity"]], temperature=ctd[["temperature"]], pressure=ctd[["pressure"]])
          expect_equal(ctd[["salinity"]], ctd_ctd[["salinity"]])
          expect_equal(ctd[["temperature"]], ctd_ctd[["temperature"]])
          expect_equal(ctd[["pressure"]], ctd_ctd[["pressure"]])
})

test_that("as.ctd() with a data frame", {
          ctd_df <- as.ctd(data.frame(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
          expect_equal(ctd[["salinity"]], ctd_df[["salinity"]])
          expect_equal(ctd[["temperature"]], ctd_df[["temperature"]])
          expect_equal(ctd[["pressure"]], ctd_df[["pressure"]])
})

test_that("as.ctd() with a list", {
          ctd_l <- as.ctd(list(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
          expect_equal(ctd[["salinity"]], ctd_l[["salinity"]])
          expect_equal(ctd[["temperature"]], ctd_l[["temperature"]])
          expect_equal(ctd[["pressure"]], ctd_l[["pressure"]])
})

test_that("ctd subsetting and trimming", {
    ## NOTE: this is brittle to changes in data(ctd), but that's a good thing, becausing
    ## changing the dataset should be done only when really necessary, e.g. the July 2015
    ## transition to use ITS-90 based temperature.
    data(ctd)
    scanRange <- range(ctd[['scan']])
    newScanRange <- c(scanRange[1] + 20, scanRange[2] - 20)
    ctdTrimmed <- ctdTrim(ctd, "scan", parameters=newScanRange)
    expect_equal(ctdTrimmed[["scan"]][1:3], c(150,151,152))
    expect_equal(ctdTrimmed[["salinity"]][1:3], c(30.8882,30.9301,30.8928))
    expect_equal(ctdTrimmed[["pressure"]][1:3], c(6.198,6.437,6.770))
    expect_equal(ctdTrimmed[["temperature"]][1:3], c(11.734383747900503536,11.630308725905782907,11.4245581060545475790))
    ## next is form a test for issue 669
    data(ctd)
    n <- length(ctd[["salinity"]])
    set.seed(669)
    lon <- ctd[["longitude"]] + rnorm(n, sd=0.05)
    lat <- ctd[["latitude"]] + rnorm(n, sd=0.05)
    ctdnew <- ctd
    ctdnew@data$longitude <- lon
    ctdnew@data$latitude <- lat
    ctdnewSubset <- subset(ctdnew, 200 <= scan & scan <= 300)
    ctdnewTrim <- ctdTrim(ctdnew, method='scan', parameters = c(200, 300))
    expect_equal(ctdnewSubset[['salinity']], ctdnewTrim[['salinity']])
    expect_equal(ctdnewSubset[['scan']], ctdnewTrim[['scan']])
    expect_equal(length(ctdnewSubset[['scan']]), length(ctdnewSubset[['longitude']]))
})

test_that("alter ctd metadata", {
    ctd[["longitude"]] <- 1
    expect_equal(ctd[["longitude"]], 1)
    ctd[["latitude"]] <- 2
    expect_equal(ctd[["latitude"]], 2)
    ## alter data
    S <- ctd[["sal"]] # tests abbreviations also
    ctd[["salinity"]] <- S + 1
    expect_equal(ctd[["salinity"]], S+1)
    top <- subset(ctd, pressure < 5)
    stopifnot(max(top[['pressure']]) < 5)
})

test_that("gsw calcuations on ctd data", {
    SP <- 35
    t <- 10
    p <- 1000
    lon <- 300
    lat <- 30
    ctd <- as.ctd(SP, t, p, longitude=lon, latitude=lat)
    expect_equal(ctd[["SP"]], SP)
    expect_equal(ctd[["t"]], t)
    expect_equal(ctd[["p"]], p)
    expect_equal(ctd[["SP"]], ctd[["salinity"]])
    expect_equal(ctd[["t"]], ctd[["temperature"]])
    expect_equal(ctd[["p"]], ctd[["pressure"]])
    Sstar <- gsw_Sstar_from_SP(SP, p=p, longitude=lon, latitude=lat)
    expect_equal(Sstar, ctd[["Sstar"]])
    SR <- gsw_SR_from_SP(SP=ctd[["SP"]])
    expect_equal(SR, ctd[["SR"]])
    SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
    expect_equal(SA, ctd[["SA"]])
    Sstar <- gsw_Sstar_from_SA(SA=SA, p=p, longitude=lon, latitude=lat)
    expect_equal(Sstar, ctd[["Sstar"]])
})
