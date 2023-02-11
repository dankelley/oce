# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

# These tests start with built-in dataset objects created with read.*()
# calls. Perhaps we should add some objects created with as.*() functions,
# too. Use a cross-pattern of velocities (east, then north, then west, and
# south), because this is easy to verify with a declination of 90 degrees.
#
# See https://github.com/dankelley/oce/issues/2038.

U <- c(1, 0, -1, 0)
V <- c(0, 1, 0, -1)
Ur <- c(0, 1, 0, -1)
Vr <- c(-1, 0, 1, 0)

# Current meter (cm-class)
test_that("applyMagneticDeclination,cm-method()", {
    data(cm)
    cm@data$u <- U
    cm@data$v <- V
    cm2 <- applyMagneticDeclination(cm, 90) # rotate clockwise 90deg
    expect_equal(cm2[["u"]], Ur)
    expect_equal(cm2[["v"]], Vr)
    expect_equal(cm2[["north"]], "geographic")
    expect_equal(cm2[["declination"]], 90)
})

# Acoustic Doppler Profiler (adp-class)
test_that("applyMagneticDeclination,adp-method()", {
    data(adp)
    adp@data$v[1:4, 1, 1] <- U
    adp@data$v[1:4, 1, 2] <- V
    adp2 <- applyMagneticDeclination(adp, 90) # rotate clockwise 90deg
    expect_equal(adp2@data$v[1:4, 1, 1], Ur)
    expect_equal(adp2@data$v[1:4, 1, 2], Vr)
    expect_equal(adp2[["north"]], "geographic")
    expect_equal(adp2[["declination"]], 90)
})

# Acoustic Doppler Velocimeter (adv-class)
test_that("applyMagneticDeclination,adv-method()", {
    data(adv)
    adv@data$v[1:4, 1] <- U
    adv@data$v[1:4, 2] <- V
    adv2 <- applyMagneticDeclination(adv, 90) # rotate clockwise 90deg
    expect_equal(adv2@data$v[1:4, 1], Ur)
    expect_equal(adv2@data$v[1:4, 2], Vr)
    expect_equal(adv2[["north"]], "geographic")
    expect_equal(adv2[["declination"]], 90)
})
