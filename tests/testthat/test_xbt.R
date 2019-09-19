## vim:textwidth=120:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data("xbt")

context("xbt (expendible bathythermograph)")

test_that("as.xbt() works", {
          XBT <- as.xbt(z=-xbt[["depth"]], temperature=xbt[["temperature"]])
          expect_equal(XBT[["units"]], list(temperature=list(unit=expression(degree*C), scale="ITS-90"),
                                            z=list(unit=expression(m), scale="")))
          expect_equal(XBT[["z"]], -xbt[["depth"]])
          expect_equal(XBT[["temperature"]], xbt[["temperature"]])
})

test_that("can read .edf file correctly", {
          XBT <- read.oce(system.file("extdata", "xbt.edf", package="oce"))
          expect_equal(XBT@metadata[c("serialNumber", "sequenceNumber", "dataNamesOriginal", "latitude",
                                      "longitude", "probeType","terminalDepth")],
                       list(serialNumber="0",
                            sequenceNumber=49L,
                            dataNamesOriginal=list(depth="Depth", temperature= "Temperature", soundSpeed="Sound Velocity"),
                            latitude=-4, longitude=4.005, probeType="T-4", terminalDepth=460))
          expect_equal(XBT[["data"]],
                       list(depth = c(5.4, 6, 6.7, 7.4, 8, 8.7),
                            temperature = c(20.91, 20.91, 20.91, 20.9, 20.9, 20.9),
                            soundSpeed = c(1575.31, 1575.32, 1575.33, 1575.34, 1575.35, 1575.36)))
})

test_that("data(xbt) and inst/extdata/xbt.edf match", {
          XBT <- read.oce(system.file("extdata", "xbt.edf", package="oce"))
          expect_equal(xbt@metadata[c("serialNumber", "sequenceNumber", "dataNamesOriginal", "latitude",
                                      "longitude", "probeType","terminalDepth")],
                       XBT@metadata[c("serialNumber", "sequenceNumber", "dataNamesOriginal", "latitude",
                                      "longitude", "probeType","terminalDepth")])
          expect_equal(XBT[["data"]], xbt[["data"]])
})

test_that("[[ works for 'z', 'depth', and 'pressure' (given depth)", {
          expect_equal(xbt[["z"]], -c(5.4, 6, 6.7, 7.4, 8, 8.7))
          expect_equal(xbt[["depth"]], c(5.4, 6, 6.7, 7.4, 8, 8.7))
          expect_equal(xbt[["pressure"]], swPressure(-xbt[["z"]], xbt[["latitude"]]))
})


