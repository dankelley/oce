# vim:textwidth=140:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data("xbt")

# See https://github.com/dankelley/oce/issues/2289#issuecomment-2628986610
test_that("read.xbt.noaa2() works", {
    expect_silent(xbts <- read.xbt.noaa2(system.file("extdata", "xbt_noaa2", package = "oce")))
    # this file holds 2 xbt profiles
    expect_equal(2L, length(xbts))
    xbt1 <- xbts[[1]]
    expect_equal(12L, length(xbt1[["pressure"]]))
    expect_equal(
        xbt1[["depth"]],
        c(0, 5, 8, 12, 17, 18, 22, 25, 30, 48, 88, 136)
    )
    expect_equal(xbt1[["temperature"]], c(15.1, 14.9, 12.9, 10.5, 7.1, 6.5, 6.1, 5.2, 4.9, 4.3, 4, 3.8))
    expect_equal(xbt1[["longitude"]], -77.25)
    expect_equal(xbt1[["latitude"]], 43.5833333333)
    expect_equal(xbt1[["time"]], as.POSIXct("1972-07-11 03:02:00", tz = "UTC"))
    # Don't bother checking everything in the second profile; just knowing that
    # the time is right ought to be enough to verify that we are handling
    # the lines in the data file correctly
    xbt2 <- xbts[[2]]
    expect_equal(xbt2[["time"]], as.POSIXct("1972-07-11 04:12:00", tz = "UTC"))
})

test_that("as.xbt() works", {
    XBT <- as.xbt(z = -xbt[["depth"]], temperature = xbt[["temperature"]])
    expect_equal(XBT[["units"]], list(
        temperature = list(unit = expression(degree * C), scale = "ITS-90"),
        z = list(unit = expression(m), scale = "")
    ))
    expect_equal(XBT[["z"]], -xbt[["depth"]])
    expect_equal(XBT[["temperature"]], xbt[["temperature"]])
})

test_that("can read .edf file correctly", {
    XBT <- read.oce(system.file("extdata", "xbt.edf", package = "oce"))
    expect_equal(
        XBT@metadata[c(
            "serialNumber", "sequenceNumber", "dataNamesOriginal", "latitude",
            "longitude", "probeType", "terminalDepth"
        )],
        list(
            serialNumber = "0",
            sequenceNumber = 49L,
            dataNamesOriginal = list(depth = "Depth", temperature = "Temperature", soundSpeed = "Sound Velocity"),
            latitude = -4, longitude = 4.005, probeType = "T-4", terminalDepth = 460
        )
    )
    expect_equal(
        XBT[["data"]],
        list(
            depth = c(5.4, 6, 6.7, 7.4, 8, 8.7),
            temperature = c(20.91, 20.91, 20.91, 20.9, 20.9, 20.9),
            soundSpeed = c(1575.31, 1575.32, 1575.33, 1575.34, 1575.35, 1575.36)
        )
    )
})

test_that("data(xbt) and inst/extdata/xbt.edf match", {
    XBT <- read.oce(system.file("extdata", "xbt.edf", package = "oce"))
    expect_equal(
        xbt@metadata[c(
            "serialNumber", "sequenceNumber", "dataNamesOriginal", "latitude",
            "longitude", "probeType", "terminalDepth"
        )],
        XBT@metadata[c(
            "serialNumber", "sequenceNumber", "dataNamesOriginal", "latitude",
            "longitude", "probeType", "terminalDepth"
        )]
    )
    expect_equal(XBT[["data"]], xbt[["data"]])
})

test_that("[[ works for 'z', 'depth', and 'pressure' (given depth)", {
    expect_equal(xbt[["z"]], -c(5.4, 6, 6.7, 7.4, 8, 8.7))
    expect_equal(xbt[["depth"]], c(5.4, 6, 6.7, 7.4, 8, 8.7))
    expect_equal(xbt[["pressure"]], swPressure(-xbt[["z"]], xbt[["latitude"]]))
})

test_that("read.xbt(type=\"sippican2\") works", {
    xbt2 <- read.xbt(system.file("extdata", "xbt2.edf", package = "oce"), type = "sippican2")
    expect_equal(xbt2[["type"]], "T-xx")
    expect_equal(xbt2[["time"]], as.POSIXct("2025-01-07 12:01:02", tz = "UTC"))
    expect_equal(xbt2[["longitude"]], -50.5)
    expect_equal(xbt2[["latitude"]], 40.5)
    expect_equal(xbt2[["serialNumber"]], "1")
    expect_equal(
        xbt2[["data"]],
        structure(list(
            time = c(0, 0.1), resistance = c(6600, 6601),
            depth = c(0, 0.5), temperature = c(18, 17.9), soundSpeed = c(
                1521,
                1520
            )
        ), class = "data.frame", row.names = c(NA, -2L))
    )
})
