# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
#
# Test a Nortek Signature100 dataset in AD2CP format that was kindly provided by
# @richardsc.  It contains burstAltimeterRaw data (unlike the dataset used by
# test_ad2cp_1.R and test_ad2cp_2.R).
#
# The tests measure consistency over time, as a guard against code changes (e.g.
# the conversion in June and July of 2022 to vectorized reading) and UI changes
# (e.g. renaming of variables).

library(oce)

file <- "local_data/ad2cp/S102791A002_Barrow_v2.ad2cp"

if (file.exists(file)) {
    test_that("local_data/ad2cp/S102791A002_Barrow_v2.ad2cp (signature 250)",
        {
            expect_warning(
                expect_warning(
                    d <- read.oce(file),
                    "using to=1477 based on file contents"),
                "'plan' defaulting to 0")
            # Identifiers
            expect_equal(d[["type"]], "Signature250")
            expect_equal(d[["fileType"]], "AD2CP")
            expect_equal(d[["serialNumber"]], 102791)
            # Entry names
            expect_equal(sort(names(d[["data"]])), c("activeConfiguration",
                    "average", "bottomTrack", "burst", "burstAltimeterRaw",
                    "orientation", "powerLevel", "status"))

            expect_equal(sort(names(d[["average"]])), c("a", "accelerometer",
                    "blankingDistance", "cellSize", "configuration",
                    "datasetDescription", "ensemble", "heading", "magnetometer",
                    "nominalCorrelation", "numberOfBeams", "numberOfCells",
                    "oceCoordinate", "orientation", "originalCoordinate", "pitch",
                    "powerLevel", "pressure", "q", "roll", "soundSpeed",
                    "temperature", "temperatureMagnetometer", "temperatureRTC",
                    "time", "transmitEnergy", "v"))

            # Beams and cells
            expect_equal(d[["oceCoordinate"]], "beam")
            # data$average
            a <- d[["average"]]
            expect_equal(d[["cellSize", "average"]], 1)
            expect_equal(d[["blankingDistance", "average"]], 0.5)
            expect_equal(d[["numberOfBeams", "average"]], 4)
            expect_equal(d[["numberOfCells", "average"]], 87)
            expect_equal(a$blankingDistance, 0.5)
            expect_equal(dim(a$v), c(360, 87, 4))
            expect_equal(dim(a$a), c(360, 87, 4))
            expect_equal(dim(a$q), c(360, 87, 4))
            expect_equal(a$v[1:3,1:3,1:3],
                structure(c(-0.553, -0.542, -0.545, 2.255, -0.448, 1.695, 2.003,
                        0.612, 2.415, 0.647, 0.641, 0.649, 1.701, 1.449, 1.762,
                        0.861, 0.971, 1.773, 0.099, 0.065, 0.096, -0.239, -0.235,
                        -0.217, -0.37, -0.365, -0.393), dim=c(3L, 3L, 3L)))
            expect_equal(a$a[1:3,1:3,1:3],
                structure(as.raw(c(0x6e, 0x6e, 0x6e, 0x34, 0x35, 0x34, 0x24, 0x22,
                            0x24, 0x70, 0x70, 0x70, 0x37, 0x37, 0x36, 0x23, 0x22,
                            0x21, 0x67, 0x67, 0x67, 0x6c, 0x6c, 0x6c, 0x4b, 0x4b,
                            0x4b)), dim=c(3L, 3L, 3L)))
            expect_equal(a$q[1:3,1:3,1:3],
                structure(as.raw(c(0x20, 0x21, 0x21, 0x15, 0x0d, 0x19, 0x1d, 0x15,
                            0x18, 0x3a, 0x3a, 0x3a, 0x11, 0x15, 0x08, 0x19, 0x1e,
                            0x16, 0x55, 0x54, 0x55, 0x58, 0x59, 0x59, 0x4b, 0x4c,
                            0x4d)), dim=c(3L, 3L, 3L)))
            expect_equal(head(a$time),
                structure(c(1653479685.6677, 1653479687.6677, 1653479689.6677,
                        1653479691.6677, 1653479693.6677, 1653479695.6677),
                    class=c("POSIXct", "POSIXt"), tzone="UTC"))

            # bottomTrack -- FIXME
            bt <- d[["bottomTrack"]] # FIXME: the values are crazy, e.g. lots of v of order e-15

            # burst -- FIXME
            b <- d[["burst"]]

            # FIXME: values are crazy, e.g. just 1 ASTDistance and ASTPressure,
            # and what else is supposed to be there?

            # FIXME: these tests talk of burstAltimeterRaw but I don't think
            # this exists in this file.

            #??? # burstAltimeterRaw
            #??? bar <- d[["burst"]]
            #??? expect_equal("beam", bar$originalCoordinate)
            #??? expect_equal("beam", bar$oceCoordinate)
            #??? # FIXME: ensemble not checked (and it's always 1, which *must* be wrong)
            #??? expect_equal(head(bar$magnetometer$x),
            #???     c(356L, 354L, 355L, 355L, 354L, 355L))
            #??? expect_equal(head(bar$magnetometer$y),
            #???     c(-310L, -310L, -310L, -311L, -309L, -310L))
            #??? expect_equal(head(bar$magnetometer$z),
            #???     c(171L, 171L, 170L, 170L, 169L, 170L))
            #??? expect_equal(head(bar$accelerometer$x),
            #???     c(-0.71142578125, -0.71142578125, -0.7119140625, -0.711669921875,
            #???         -0.71142578125, -0.711669921875))
            #??? expect_equal(head(bar$accelerometer$y),
            #???     c(0.70111083984375, 0.70086669921875, 0.70086669921875,
            #???         0.70086669921875, 0.70111083984375, 0.70062255859375))
            #??? expect_equal(head(bar$accelerometer$z),
            #???     c(0.01654052734375, 0.01654052734375, 0.01654052734375,
            #???         0.01654052734375, 0.01654052734375, 0.01678466796875))
            #??? expect_equal(1833L, bar$altimeterRawNumberOfSamples)
            #??? expect_equal(bar$altimeterRawSamples[1:3,1:3],
            #???     structure(c(2313L, 1542L, 2560L, 2560L, 2442L, 3150L, 2313L, 4570L,
            #???             3579L), dim = c(3L, 3L)))
            #??? expect_equal(head(bar$altimeterDistance),
            #???     c(42.0308685302734, 42.1037330627441, 42.1036491394043, 40.7197113037109,
            #???         42.1040687561035, 40.7203636169434))
            #??? expect_equal(head(bar$pressure),
            #???     c(0.807, 0.801, 0.8, 0.792, 0.808, 0.796))
            #??? expect_true(all("zup" == bar[["orientation"]]))
            #??? expect_equal(head(bar$ASTDistance),
            #???     c(42.1686553955078, 42.2458877563477, 42.2420387268066,
            #???         40.852237701416, 42.2431640625, 40.8575706481934))
            #??? expect_equal(head(bar$ASTPressure),
            #???     c(0.806999981403351, 0.800999999046326, 0.800000011920929,
            #???         0.791999995708466, 0.808000028133392, 0.796000003814697))

            #??? # Time
            #??? expect_equal(head(d[["time"]]),
            #???     structure(c(1653479685.6677, 1653479687.6677, 1653479689.6677,
            #???             1653479691.6677, 1653479693.6677, 1653479695.6677), class =
            #???         c("POSIXct", "POSIXt"), tzone = "UTC"))
            #??? expect_equal(d[["time"]], d[["time", "average"]])

            #??? # Pressure, temperature etc
            #??? expect_equal(head(d[["pressure"]]),
            #???     c(0.791, 0.801, 0.807, 0.796, 0.803, 0.799))
            #??? expect_equal(d[["pressure", "average"]], d[["pressure"]])
            #??? expect_equal(head(d[["temperature"]]),
            #???     c(20.18, 20.18, 20.18, 20.18, 20.18, 20.18))
            #??? expect_equal(d[["temperature"]], d[["temperature", "average"]])
            #??? expect_equal(head(d[["heading"]]),
            #???     c(280.21, 279.75, 279.97, 279.75, 279.72, 279.82))
            #??? expect_equal(d[["heading"]], d[["heading", "average"]])
            #??? expect_equal(head(d[["pitch"]]),
            #???     c(-45.4, -45.43, -45.43, -45.41, -45.42, -45.42))
            #??? expect_equal(d[["pitch"]], d[["pitch", "average"]])

            #??? expect_equal(head(d[["roll"]]),
            #???     c(88.64, 88.62, 88.62, 88.64, 88.64, 88.62))
            #??? expect_equal(d[["roll"]], d[["roll", "average"]])
        })
}

