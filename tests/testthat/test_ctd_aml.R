# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)
file <- system.file("extdata", "ctd_aml.csv", package="oce")

# https://github.com/dankelley/oce/issues/1891
test_that("read.ctd.aml() works (issue 1924)", {
    ctd <- read.ctd.aml(file)
    expect_equal(ctd[["longitude"]], -145.8477)
    expect_equal(ctd[["latitude"]], 70.2284 )
    expect_true(is.na(ctd[["serialNumber"]]))
    expect_equal(length(ctd[["header"]]), 109L)
    expect_equal(head(ctd[["pressure"]],3),
        c(0.23, 0.22, 0.22))
    expect_equal(head(ctd[["salinity"]],3),
        c(5.57787090635139, 9.97826094600314, 14.628766628731))
    expect_equal(head(ctd[["temperature"]],3),
        c(5.671, 5.509, 5.4))
})

