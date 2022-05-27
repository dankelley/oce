# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Test Sun & Sea CTD data file.
# https://github.com/dankelley/oce/discussions/1929
library(oce)
test_that("read.ctd.ssda() works (discussions/1929)", {
    file <- system.file("extdata", "ctd_ssda.csv", package="oce")
    ctd <- read.oce(file)
    expect_equal(ctd[["longitude"]], 15.9990216666667)
    expect_equal(ctd[["latitude"]], 55.292085)
    expect_equal(length(ctd[["header"]]), 47L)
    expect_equal(ctd[["salinity"]],
        c(0.08, 0.11, 0.12, 0.12, 0.11, 0.11))
    expect_equal(ctd[["temperature"]],
        c(5.007, 4.999, 4.993, 4.988, 4.984, 4.981))
    expect_equal(ctd[["pressure"]],
        c(0, -0.01, -0.02, -0.01, -0.02, -0.02))
})


