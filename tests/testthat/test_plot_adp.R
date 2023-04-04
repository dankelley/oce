# vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data(adp)

test_that("test all 'which' values listed in ?'plot,adp-method' on 2019 May 23", {
    # Most of the numerical tests have character equivalents, but we
    # test both. All this test really does is to ensure that the plots
    # do not produce warnings, apart from known things, e.g. a warning
    # if trying to plot bottom-tracking items, because data(adp) has
    # no bottom-tracking data.
    for (which in c(1:4, 5:8, 9:12, 60, 70:73, 80:83, 13, 14, 15:18,
            19:22, 23, 24:28, 29, 30, 40, 41:44, 50, 51:54, 55,
            100)) {
        if (which %in% c(40:44, 50:54))
            expect_error(plot(adp, which=which),
                "ADP object lacks bottom-tracking data")
        else if (which == 80)
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vv' data item")
        else if (which == 81)
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'va' data item")
        else if (which == 82)
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vq' data item")
        else if (which == 83)
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vg' data item")
        else
            expect_silent(plot(adp, which=which))
    }
    for (which in c("u1", "u2", "u3", "u4", "a1", "a2", "a3", "a4", "q1",
            "q2", "q3", "q4", "map", "g1", "g2", "g3", "g4", "vv",
            "va", "vq", "vg", "vertical", "salinity",
            "temperature", "pressure", "heading", "pitch", "roll",
            "progressiveVector", "uv", "uv+ellipse",
            "uv+ellipse+arrow", "bottomRange", "bottomRange1",
            "bottomRange2", "bottomRange3", "bottomRange4",
            "bottomVelocity1", "bottomVelocity2",
            "bottomVelocity3", "bottomVelocity4",
            "bottomVelocity", "heaving", "soundSpeed", "velocity",
            "amplitude", "quality", "hydrography", "angles")) {
        if (length(grep("bottom", which)))
            expect_error(plot(adp, which=which),
                "ADP object lacks bottom-tracking data")
        else if (which == "vertical")
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vv' data item")
        else if (which == "vv")
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vv' data item")
        else if (which == "va")
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'va' data item")
        else if (which == "vq")
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vq' data item")
        else if (which == "vg")
            expect_error(plot(adp, which=which),
                "ADP object lacks a 'vg' data item")
        else
            expect_silent(plot(adp, which=which))
    }
})

test_that("some specialized plot types", {
    expect_silent(plot(adp, which=23, control=list("bin"=1)))
    expect_silent(plot(adp))
})
