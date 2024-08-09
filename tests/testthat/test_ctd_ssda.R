# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# Test Sun & Sea CTD data file.
# https://github.com/dankelley/oce/discussions/1929
#
# NOTE 2022-08-16: I moved this to a non-CRAN test because it fails on the CRAN
# test machine labelled as r-devel-linux-x86_64-debian-clang.  Debugging for
# that machine is not easy, given that rhub hasn't offered a supposedly
# analagous machine for weeks.  I moved the test to be local, so at least
# developers can check it, without risking CRAN rejection for a problem.

library(oce)
file <- "local_data/ctd_ssda.csv"
if (file.exists(file)) {
    skip_on_cran()
    test_that("read.ctd.ssda() works (discussions/1929; issues/2227)", {
        ctd <- read.oce(file)
        expect_equal(ctd[["longitude"]], 15.9990216666667)
        expect_equal(ctd[["latitude"]], 55.292085)
        expect_equal(length(ctd[["header"]]), 47L)
        expect_equal(
            ctd[["salinity"]],
            c(0.08, 0.11, 0.12, 0.12, 0.11, 0.11)
        )
        expect_equal(
            ctd[["temperature"]],
            c(5.007, 4.999, 4.993, 4.988, 4.984, 4.981)
        )
        expect_equal(
            ctd[["pressure"]],
            c(0, -0.01, -0.02, -0.01, -0.02, -0.02)
        )
    })
}
