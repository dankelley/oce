# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# NOTE: the read.*() tests are not run on windows machines, because some of the
# remote checks I've done on those machines fail, and so including the tests
# here might cause problems with the CRAN checks. The issues seem to relate to
# the encoding of the files. See https://github.com/dankelley/oce/issues/1927
# for discussion.  From reading R developer forums, I am of the impression that
# the handling of encoding is changing in windows systems.
#
# Note that the csv3 file starts with <U+FEFF>, and these two bytes signify a
# "byte order mark".  So, ought we to read that with UTF-8-BOM or with latin1
# encoding?  I've done experiments with both.  I think the present code, as of
# 2022-03-28, works properly on macos, various linux flavours, and *some*
# windows machines ... but not on *all* windows machines. Therefore, I have
# turned testing off for all windows machines.


library(oce)

csv1 <- system.file("extdata", "test_met_vsn1.csv", package="oce")
csv2 <- system.file("extdata", "test_met_vsn2.csv", package="oce")
csv3 <- system.file("extdata", "test_met_vsn3.csv", package="oce")
xml2 <- system.file("extdata", "test_met_xml2.xml", package="oce")

test_that("oceMagic understands all types of met data files", {
    expect_equal("met/csv1", oceMagic(csv1))
    expect_equal("met/csv2", oceMagic(csv2))
    expect_equal("met/csv3", oceMagic(csv3))
    expect_equal("met/xml2", oceMagic(xml2))
})

if (.Platform$OS.type != "windows") {
    test_that("read.met() handles type=\"csv1\" files", {
        d <- read.met(csv1)
        expect_equal(names(d@data), c("dataQuality", "dewPoint",
                "direction", "humidex", "humidity",
                "pressure", "speed", "temperature",
                "time", "u", "v", "visibility",
                "weather", "wind", "windChill"))
        expect_equal(d[["latitude"]], 44.88)
        expect_equal(d[["longitude"]], -63.5)
        expect_equal(d[["elevation"]], 145.4)
        expect_equal(d[["station"]], "HALIFAX STANFIELD INT'L A")
        expect_equal(d[["climateIdentifier"]], "8202250")
        expect_equal(d[["WMOIdentifier"]], "71395")
        expect_equal(d[["TCIdentifier"]], "YHZ")
        expect_equal(d[["temperature"]], c(12.1, 11.8, 11.4, 10.9, 10.9))
        expect_equal(d[["humidity"]], c(77, 76, 74, 73, 76))
        expect_equal(d[["wind"]], c(7, 7, 6, 6, 6))
        expect_equal(d[["u"]], c(-6.650391676e-01, -6.650391676e-01, 2.041077999e-16,
                1.071312683e+00, 1.276740739e+00))
        expect_equal(d[["v"]], c(1.827180096, 1.827180096, 1.666666667,
                1.276740739, 1.071312683))
        expect_equal(d[["time"]], as.POSIXct(c("2003-09-01 00:00:00", "2003-09-01 01:00:00",
                    "2003-09-01 02:00:00", "2003-09-01 03:00:00",
                    "2003-09-01 04:00:00"), tz="UTC"))
})
}

#test_that("read.oce(file) and read.met(file, type=\"csv1\") give same metadata and data slots", {
#    doce <- read.oce(csv1)
#    dmet <- read.met(csv1)
#    expect_equal(doce[["data"]], dmet[["data"]])
#    moce <- doce[["metadata"]]
#    mmet <- dmet[["metadata"]]
#    expect_equal(moce[names(moce)!="filename"], mmet[names(mmet)!="filename"])
#})

if (.Platform$OS.type != "windows") {
    test_that("read.met() handles type=\"csv1\" files", {
        expect_silent(d <- read.met(csv1))
        expect_equal(sort(names(d[["data"]])),
            c("dataQuality", "dewPoint", "direction", "humidex",
                "humidity", "pressure", "speed", "temperature",
                "time", "u", "v", "visibility", "weather", "wind",
                "windChill"))
})
}

if (.Platform$OS.type != "windows") {
    test_that("read.met() handles type=\"csv3\" files", {
        expect_silent(d <- read.met(csv3))
        # Sort both because the ordering is different when done interactively
        # and in the test (for reasons I don't understand).
        expect_equal(sort(names(d@data)), sort(c(paste("Date/Time", "(LST)"),
                    "dewPoint", "direction", "humidex", "humidity",
                    "precipitation",  "pressure", "speed", "temperature",
                    "time", paste("Time", "(LST)"), "u", "v", "visibility",
                    "weather", "windChill")))

        expect_equal(d[["latitude"]], 44.88)
        expect_equal(d[["longitude"]], -63.51)
        expect_equal(d[["station"]], "HALIFAX STANFIELD INT'L A")
        expect_equal(d[["temperature"]], c(1.7, 1.9, 2, 2, 2.4))
        expect_equal(d[["humidity"]], rep(100L, 5))
        expect_equal(d[["speed"]], c(2.77777777777778, 2.77777777777778,
                3.05555555555556, 2.77777777777778, 3.61111111111111))
        expect_equal(d[["u"]], c(2.12790123088605, 1.78552113801816,
                2.34069135397465, 1.38888888888889, 2.76627160015187))
        expect_equal(d[["v"]], c(-1.78552113801816, -2.12790123088605,
                -1.96407325181998, -2.40562612162344, -2.32117747942361))
        expect_equal(d[["time"]], as.POSIXct(c("2022-01-01 00:00:00",
                    "2022-01-01 01:00:00", "2022-01-01 02:00:00",
                    "2022-01-01 03:00:00", "2022-01-01 04:00:00"),
                tz="UTC"))
})
}

## Test removed 2022-03-27 because read.oce() does not try as hard to
## decode met column names is read.met() does.  It's fine for read.met()
## to do extra work, but it's risky for read.oce() to be polluted with
## weird special cases, such as we see in the evolving files from
## Environment Canada, which have multiple choices on (a) whether
## to use a byte-order mark, whether to use parentheses on unit,
## whether to use a degree symbol on temperatures, etc.
#test_that("read.oce(file) and read.met(file, type=\"csv2\") give same metadata and data slots", {
#    doce <- read.oce(csv2)
#    dmet <- read.met(csv2)
#    expect_equal(doce[["data"]], dmet[["data"]])
#    moce <- doce[["metadata"]]
#    mmet <- dmet[["metadata"]]
#    expect_equal(moce[names(moce)!="filename"], mmet[names(mmet)!="filename"])
#})

if (requireNamespace("XML", quietly=TRUE)) {
    test_that("read.oce(file) and read.met(file, type=\"xml2\") give same metadata and data slots", {
        doce <- read.oce(xml2)
        dmet <- read.met(xml2)
        expect_equal(doce[["data"]], dmet[["data"]])
        moce <- doce[["metadata"]]
        mmet <- dmet[["metadata"]]
        expect_equal(moce[names(moce)!="filename"], mmet[names(mmet)!="filename"])
})
}
