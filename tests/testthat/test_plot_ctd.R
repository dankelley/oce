# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

test_that("plotTS() handles differently EOSs correctly", {
    data(ctd)
    options(oceEOS="unesco")
    expect_silent(plotTS(ctd))
    expect_silent(plotTS(ctd, eos="unesco"))
    expect_silent(plotTS(ctd, eos="gsw"))
    options(oceEOS="gsw")
    expect_silent(plotTS(ctd))
    expect_silent(plotTS(ctd, eos="unesco"))
    expect_silent(plotTS(ctd, eos="gsw"))
})


test_that("ctd subsetting and trimming", {
    # NOTE: this is brittle to changes in data(ctd), but that's a good thing, because
    # changing the dataset should be done only when really necessary, e.g. the July 2015
    # transition to use ITS-90 based temperature (because IPTS-68 was not
    # yet handled by oce at that time), and the April 2016
    # transition back to IPTS-68 for this dataset, once oce could handle
    # both scales.
    #
    # 1. SBE trimming method
    pressure <- c(rep(4, 1000),
        seq(4, 0.5, length.out = 50),
        seq(0.5, 100, length.out=1000),
        rep(100, 100),
        seq(100, 0, length.out=1000))
    salinity <- 35 - pressure / 100
    temperature <- 10 + (100 - pressure) / 50
    d <- as.ctd(salinity, temperature, pressure)
    plotScan(d)
    dt <- ctdTrim(d, method="sbe")
    dt2 <- ctdTrim(d)
    #  2. trim by scan
    scanRange <- range(ctd[["scan"]])
    newScanRange <- c(scanRange[1] + 20, scanRange[2] - 20)
    ctdTrimmed <- ctdTrim(ctd, "scan", parameters=newScanRange)
    # below are the data at the top of this trim spot
    # 150    149.000      6.198      6.148    11.7372    30.8882  0.000e+00
    # 151    150.000      6.437      6.384    11.6331    30.9301  0.000e+00
    # 152    151.000      6.770      6.715    11.4273    30.8928  0.000e+00
    expect_equal(ctdTrimmed[["scan"]][1:3], c(150, 151, 152))
    expect_equal(ctdTrimmed[["salinity"]][1:3], c(30.8882, 30.9301, 30.8928))
    expect_equal(ctdTrimmed[["pressure"]][1:3], c(6.198, 6.437, 6.770))
    # The next two lines check on whether the data in the object match
    # exactly the data file, and then whether the accessor converts from
    # the old temperature scale that is in the data file to the new one
    # that formulas are based on. There are changes here to account for
    # a change to data(ctd) that addressed a bug discussed in issue 1293.
    expect_equal(ctdTrimmed@data$temperature[1:3], c(11.7372, 11.6331, 11.4273))
    expect_equal(ctdTrimmed[["temperature"]][1:3], T90fromT68(c(11.7372, 11.6331, 11.4273)))
    # next is from a test for issue 669
    n <- length(ctd[["salinity"]])
    set.seed(669)
    lon <- ctd[["longitude"]] + rnorm(n, sd=0.05)
    lat <- ctd[["latitude"]] + rnorm(n, sd=0.05)
    ctdnew <- ctd
    # change from one-location object to multi-location one
    ctdnew@metadata$longitude <- NULL
    ctdnew@data$longitude <- lon
    ctdnew@metadata$latitude <- NULL
    ctdnew@data$latitude <- lat
    ctdnewSubset <- subset(ctdnew, 200 <= scan & scan <= 300)
    ctdnewTrim <- ctdTrim(ctdnew, method="scan", parameters = c(200, 300))
    expect_equal(ctdnewSubset[["salinity"]], ctdnewTrim[["salinity"]])
    expect_equal(ctdnewSubset[["scan"]], ctdnewTrim[["scan"]])
    expect_equal(length(ctdnewSubset[["scan"]]), length(ctdnewSubset[["longitude"]]))
})

# Below an excerpt from the docs, as of 2022-03-01
# 1 "salinity+temperature"
# 2 "density+N2"
# 3 "TS"
# 4 "text"
# 5 "map"
# 5.1 as for which=5, except that the file name is drawn above the map.
# 6 "density+dpdt"
# 7 "density+time"
# 8 "index"
# 9 "salinity"
# 10 "temperature"
# 11 "density"
# 12 "N2"
# 13 "spice"
# 14 "tritium"
# 15 "Rrho"
# 16 "RrhoSF"
# 17 "conductivity"
# 20 "CT"
# 21 "SA"
# 30 "Sts"
# 31 "Tts"
# 32 "pts"
# 33 "rhots"
test_that("erroneous numeric 'which' is handled", {
    expect_error(plot(ctd, which=999), "which=\"999\" cannot be handled")
})

test_that("erroneous textual 'which' is handled", {
    expect_error(plot(ctd, which="unhandled"), "which=\"unhandled\" cannot be handled")
})

test_that("numeric 'which' works", {
    for (w in c(5.1, 1:13, 15:17, 20:21, 30:33)) {
        expect_silent(plot(ctd, which=w))
    }
})

test_that("character 'which' works", {
    for (w in c("salinity+temperature", "density+N2", "TS", "text", "map",
            "density+dpdt", "density+time", "index", "salinity", "temperature",
            "density", "N2", "spice", "Rrho", "RrhoSF",
            "conductivity", "CT", "SA", "Sts", "Tts", "pts", "rhots")) {
        expect_silent(plot(ctd, which=w))
    }
})
