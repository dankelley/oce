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
H <- 1:4
Hr <- H + 90

# Current meter (cm-class)
test_that("applyMagneticDeclination,cm-method()", {
    data(cm)
    cm@data$u <- U
    cm@data$v <- V
    cm@data$Hdg <- H
    cm@data$Hdg.1 <- H
    cm2 <- applyMagneticDeclination(cm, 90) # rotate clockwise 90deg
    expect_equal(cm2[["u"]], Ur)
    expect_equal(cm2[["v"]], Vr)
    expect_equal(cm2[["Hdg"]], Hr)
    expect_equal(cm2[["Hdg.1"]], Hr)
    expect_equal(cm2[["north"]], "geographic")
    expect_equal(cm2[["declination"]], 90)
})

# Acoustic Doppler Profiler (adp-class)
test_that("applyMagneticDeclination,adp-method()", {
    data(adp)
    adp@data$v[1:4, 1, 1] <- U
    adp@data$v[1:4, 1, 2] <- V
    adp@data$heading[1:4] <- H
    adp2 <- applyMagneticDeclination(adp, 90) # rotate clockwise 90deg
    expect_equal(adp2@data$v[1:4, 1, 1], Ur)
    expect_equal(adp2@data$v[1:4, 1, 2], Vr)
    expect_equal(adp2@data$heading[1:4], Hr)
    expect_equal(adp2[["north"]], "geographic")
    expect_equal(adp2[["declination"]], 90)
})

# Acoustic Doppler Velocimeter (adv-class)
test_that("applyMagneticDeclination,adv-method()", {
    data(adv)
    adv@data$v[1:4, 1] <- U
    adv@data$v[1:4, 2] <- V
    adv@data$heading[1:4] <- H
    adv2 <- applyMagneticDeclination(adv, 90) # rotate clockwise 90deg
    expect_equal(adv2@data$v[1:4, 1], Ur)
    expect_equal(adv2@data$v[1:4, 2], Vr)
    expect_equal(adv2@data$heading[1:4], Hr)
    expect_equal(adv2[["north"]], "geographic")
    expect_equal(adv2[["declination"]], 90)
})

test_that("various ways of handling declination (real-world RDI data)", {
    # adp1 = specify declination when converting from xyz
    # adp2 = set declination=0 when converting from xyz
    # adp3 = specify declination after converting from xyz
    declination <- -18.036
    file <- system.file("extdata", "adp_rdi.000", package = "oce")
    beam <- read.oce(file, from = 1, to = 4)
    xyz <- beamToXyzAdp(beam)
    adp1 <- xyzToEnuAdp(xyz, declination = declination)
    adp2 <- xyzToEnuAdp(xyz)
    expect_warning(
        adp3 <- applyMagneticDeclination(adp2, declination = declination),
        "a declination has already been applied"
    )
    # beam@metadata should not contain 'declination' or 'north'
    expect_null(beam[["declination"]])
    expect_null(beam[["north"]])
    # adp1@metadata
    expect_equal(adp1[["declination"]], declination)
    expect_equal(adp1[["north"]], "geographic")
    # adp2@metadata
    expect_equal(adp2[["declination"]], 0.0)
    expect_equal(adp2[["north"]], "geographic")
    # adp3@metadata
    expect_equal(adp3[["declination"]], declination)
    expect_equal(adp3[["north"]], "geographic")
    # adp1 specifies declination early, adp3 late; expect equal
    expect_equal(adp1[["heading"]], adp3[["heading"]])
    # beam and adp2 should have same heading
    expect_equal(beam[["heading"]], adp2[["heading"]])
    # is declination right in adp2?
    expect_equal(adp1[["heading"]], adp2[["heading"]] + declination)
    # is declination right in adp3?
    expect_equal(adp3[["heading"]], adp2[["heading"]] + declination)
    # velocities
    # Do adp1 and adp3 match?
    expect_equal(adp1[["v"]], adp3[["v"]])
    # Are components 3 and 4 unaltered?
    for (component in 3:4) {
        expect_equal(adp1[["v"]][, , component], adp2[["v"]][, , component])
        expect_equal(adp1[["v"]][, , component], adp3[["v"]][, , component])
    }
    # Is rotated adp2 (called A) same as adp1 (called B)?
    S <- sin(-declination * pi / 180)
    C <- cos(-declination * pi / 180)
    rot <- matrix(c(C, S, -S, C), nrow = 2)
    for (bin in seq_len(dim(adp1@data$v)[2])) {
        A <- rot %*% rbind(adp2@data$v[, bin, 1], adp2@data$v[, bin, 2])
        B <- rbind(adp1@data$v[, bin, 1], adp1@data$v[, bin, 2])
        expect_equal(A, B)
    }
})
