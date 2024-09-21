# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

test_that("read.ctd.aml() works for a type 1 file (issue 1924)", {
    file <- system.file("extdata", "ctd_aml_type1.csv.gz", package = "oce")
    ctd <- read.ctd.aml(file)
    expect_equal(ctd[["longitude"]], -145.8477)
    expect_equal(ctd[["latitude"]], 70.2284)
    expect_true(is.na(ctd[["serialNumber"]]))
    expect_equal(length(ctd[["header"]]), 109L)
    expect_equal(head(ctd[["pressure"]], 3), c(0.23, 0.22, 0.22))
    expect_equal(head(ctd[["salinity"]], 3), c(5.57787090635139, 9.97826094600314, 14.628766628731))
    expect_equal(head(ctd[["temperature"]], 3), c(5.671, 5.509, 5.4))
})

test_that("read.ctd.aml() works for a type 3 file (issue 2247)", {
    file <- system.file("extdata", "ctd_aml_type3.csv.gz", package = "oce")
    ctd <- read.ctd.aml(file)
    expect_equal(ctd[["longitude"]], 15.2466)
    expect_equal(ctd[["latitude"]], 43.8183)
    expect_equal(length(ctd[["header"]]), 21L)
    expect_equal(ctd[["pressure"]], c(0.00, 0.19, 0.44, 0.68, 0.90))
    expect_equal(ctd[["salinity"]], c(37.797, 37.907, 37.914, 37.92, 37.932))
    expect_equal(ctd[["temperature"]], c(25.871, 25.89, 25.861, 25.862, 25.838))
})
