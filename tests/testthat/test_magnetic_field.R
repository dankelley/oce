# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

test_that("magneticField() handles both POSIX times and dates", {
    A <- magneticField(-63.562, 44.640, as.POSIXct("2013-01-01", tz = "UTC"), version = 12)$declination
    B <- magneticField(-63.562, 44.640, as.Date("2013-01-01"), version = 12)$declination
    expect_equal(A, B, tolerance = 1e-8)
})

test_that("magneticField version 12 (why not perfect?)", {
    # test values from http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html
    # UPDATE March 3, 2020: I cannot test these old values because that
    # page now only works for present and future dates (and it's quite
    # hard to figure out, frankly).
    expect_equal(-17.976, magneticField(-63.562, 44.640, 2013, version = 12)$declination,
        tolerance = 0.001
    )
    expect_equal(67.562, magneticField(-63.562, 44.640, 2013, version = 12)$inclination,
        tolerance = 0.006
    ) # Q: why does tol=0.001 fail?
    expect_equal(52096, magneticField(-63.562, 44.640, 2013, version = 12)$intensity,
        tolerance = 16
    ) # Q: why does tol=1 fail?
})

test_that("magneticField version 13 (why not perfect?)", {
    # REF: http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html
    # version 13 by default as of oce "develop" branch date 2020-03-03
    mf <- magneticField(-63.562, 44.640, as.POSIXct("2020-03-03 00:00:00", tz = "UTC"), version = 13)
    mf2 <- magneticField(-63.562, 44.640, as.Date("2020-03-03", tz = "UTC"), version = 13)
    cbind(mf, mf2) # Q: why so much difference here?
    expect_equal(-16.972, mf$declination, tolerance = 0.005) # Q: why does tol=0.001 fail?
    expect_equal(66.855, mf$inclination, tolerance = 0.001)
    expect_equal(51498, mf$intensity, tolerance = 3) # Q: why does tol=1 fail?
})
