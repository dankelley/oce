# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Test SAIV A/V file; see
# https://github.com/dankelley/oce/discussions/2141

# CRAN will ignore the test since local_data is in .Rbuildignore

library(oce)
file <- "local_data/ctd/saiv.txt"
if (file.exists(file)) {
    test_that("read.ctd.saiv() works (issues/2141)", {
        ctd <- read.ctd.saiv(file)
        # Checks against what I see by eye in the file.
        expect_equal(
            names(ctd[["data"]]),
            c(
                "series", "measurement", "salinity", "temperature", "fluorescence",
                "turbidity", "sigma", "soundVelocity", "depth", "Date", "Time"
            )
        )
        expect_equal(
            head(ctd[["salinity"]], 4L),
            c(0.02, 0.01, 0.02, 0.01)
        )
        expect_equal(
            head(ctd[["temperature"]], 4L),
            c(8.221, 8.129, 8.127, 8.115)
        )
        expect_equal(max(ctd[["depth"]]), 20.76)
        expect_equal(max(ctd[["pressure"]]), swPressure(20.76))
    })
}
