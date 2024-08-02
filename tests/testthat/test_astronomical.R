# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# References used in this file:
##
# 1. Meeus, Jean, 1982. Astronomical formulae for calculators. Willmann-Bell. Richmond VA, USA. 201 pages.
# 2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA, USA. 429 pages.

library(oce)

RPD <- atan2(1, 1) / 45 # radians per degree

test_that("Angles", {
    # A randomly-chosen example on page 99 of Meeus (1991).
    # Hundreds of other cases could be found in that book, or his
    # earlier one, but the fomulae is self-evident so a single
    # value ought to be enough to ensure against editing errors that
    # might accidentally alter degree2hms().
    hms <- angle2hms(177.74208)
    expect_equal(hms$hourDecimal, 177.74208 * 24 / 360)
    expect_equal(hms$hour, 11)
    expect_equal(hms$minute, 50)
    expect_equal(hms$second, 58.0992)
    expect_equal(hms$string, "11h50m58s.10")
})


test_that("Times", {
    # [1] chapter 3 page 24-25
    # FIXME: previously this had the unintelligble tz="ET" but it is *exact* as is
    t <- ISOdatetime(1957, 10, 4, hour = 0, min = 0, sec = 0, tz = "UTC") + 0.81 * 86400
    expect_equal(julianDay(t), 2436116.31, tolerance = 0.01)
    # [1] example 15.a
    t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz = "UTC")
    jd <- julianDay(t)
    jca <- julianCenturyAnomaly(jd)
    expect_equal(jd, 2443825.69, tolerance = 0.01)
    expect_equal(jca, 0.788656810, tolerance = 1e-7) # fractional error 3e-8
    # [1] page 40
    t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz = "UTC")
    expect_equal(siderealTime(t), 3.4503696, tolerance = 0.0000001)
    t <- ISOdatetime(1978, 11, 13, 4, 34, 0, tz = "UTC")
    expect_equal(siderealTime(t), 8.0295394, tolerance = 0.0000001)
})

test_that("Moon", {
    # [2] example 45.a (pages 312-313)
    # Do not check too many digits, because the code does not have all terms
    # in formulae.
    # NB. this block also tests eclipticalToEquatorial(), julianDay(),
    # and julianCenturyAnomaly().
    t <- ISOdatetime(1992, 04, 12, 0, 0, 0, tz = "UTC")
    m <- moonAngle(t, 0, 0) # lat and lon arbitrary
    expect_equal(m$lambda, 133.162659, tolerance = 0.0002)
    expect_equal(m$beta, -3.229127, tolerance = 0.0002)
    ## expect_equal(abs(m$obliquity - 23.440636) < 0.001)
    expect_equal(m$rightAscension, 134.688473, tolerance = 0.02)
    expect_equal(m$declination, 13.768366, tolerance = 0.001)
    expect_equal(m$diameter, 0.991990, tolerance = 0.0001)
    expect_equal(m$distance, 368405.6, tolerance = 0.1)
    # moon illuminated fraction [1] ex 31.b page 156
    illfrac <- (1 + cos(RPD * 105.8493)) / 2
    expect_equal(moonAngle(ISOdatetime(1979, 12, 25, 0, 0, 0, tz = "UTC"), 0, 0)$illuminatedFraction, illfrac, tolerance = 0.001)
    # Local time
    tlocal <- t
    attributes(tlocal)$tzone <- ""
    mlocal <- moonAngle(tlocal, 0, 0)
    expect_identical(m, mlocal)
    # Numerical time
    expect_identical(m, moonAngle(as.numeric(t), 0, 0))
    # time as a Sys.Date value
    moonAngle(Sys.Date(), -63.5744, 44.6479)
    # time as a character value
    moonAngle("2019-01-20", -63.5744, 44.6479)
    # How close to "total full" was the time of the lunar eclipse for the
    # "super Blood full moon" of 2019, in Halifax, NA?
    expect_gt(moonAngle("2019-01-21 00:12:00", -63.5744, 44.6479)$illuminatedFraction, 0.999)
})

test_that("Sun", {
    # Testing against values that worked on 2016-12-06;
    # FIXME: replace by numbers from [2] if they can be found.
    t <- ISOdatetime(1992, 04, 12, 0, 0, 0, tz = "UTC")
    s <- sunAngle(t, 0, 0) # lat and lon arbitrary
    expect_equal(s$azimuth, 358.624168865654)
    expect_equal(s$altitude, -81.3030909018573)
    expect_equal(s$diameter, 0.531871759139675)
    expect_equal(s$distance, 1.00249729533012)
    # Local time
    tlocal <- t
    attributes(tlocal)$tzone <- ""
    slocal <- sunAngle(tlocal, 0, 0)
    expect_identical(s, slocal)
    # Numerical time
    expect_identical(s, sunAngle(as.numeric(t), 0, 0))
    # Character time
    expect_identical(s, sunAngle("1992-04-12 00:00:00", 0, 0))
})

test_that("Sun Declination and Right Ascension", {
    # Example 24.a in Meeus (1991) (page 158 PDF, 153 print)
    # This is *apparent* declination and right ascension
    time <- as.POSIXct("1992-10-13 00:00:00", tz = "UTC")
    a <- sunDeclinationRightAscension(time, apparent = TRUE)
    expect_equal(a$declination, -7.78507, tolerance = 0.00004)
    expect_equal(a$rightAscension, -161.61919, tolerance = 0.00003)
    b <- sunDeclinationRightAscension(time)
    # check against previous results, to protect aginst code-drift errors
    # This is *actual* declination and right ascension
    expect_equal(b$declination, -7.785464443, tolerance = 0.000000001)
    expect_equal(b$rightAscension, -161.6183305, tolerance = 0.0000001)
})

test_that("Handle multiples correctly (issue 2178)", {
    d <- data.frame(
        t = c("2023-01-01 00:00", "2024-01-01 01:00", "2023-01-01 02:00"),
        lat = c(45, 46, 47),
        lon = c(-65, -66, -67),
        ref = rep(TRUE, 3)
    )
    a <- data.frame(with(d, sunAngle(t, lon, lat, ref)))
    for (i in seq_len(nrow(d))) {
        ai <- data.frame(with(d[i, ], sunAngle(t, lon, lat, ref)))
        for (name in names(a)) {
            expect_equal(a[i, name], ai[[name]])
        }
    }
})

test_that("Meeuse Ex 45.a", {
    # Test against example 45a (page 312) in Meeuse (1991). Note
    # that those values are for an updated set of formulae, and
    # this is not what we have in oce (which uses Meeuse 1982
    # formulae.
    #
    # I set up these tests to help me to decide whether to recode
    # oce to the new formulae.  I think the differences are too
    # small to justify that effort, which is quite substantial,
    # involving typing a lot of formulae and numbers in from a book.
    #
    # I am not using expect_equal() because I want to display
    # the mismatch concretely.
    #
    # Summary: the worst differences are under 0.01 degrees,
    # which I judge to be tolerable for simple tasks.
    t <- as.POSIXct("1992-04-12 00:00:00", tz = "UTC")
    ma <- moonAngle(t, longitude = 0, latitude = 0)
    expect_true(abs(ma$rightAscension - 134.688473) < 0.017) # alpha='apparent right ascension'
    expect_true(abs(ma$declination - 13.768366) < 0.0046) # delta='declinaiton'
    expect_true(abs(ma$lambda - 133.162659) < 0.013) # longitude
    expect_true(abs(ma$beta - (-3.229127)) < 0.00011)
    expect_true(abs(ma$distance - 368409.7) < 7.5) # delta='distance [km]'
    expect_true(abs(ma$diameter - 0.991990) < 0.000021) # pi
})
