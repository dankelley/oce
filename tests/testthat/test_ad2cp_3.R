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
    test_that("local_data/ad2cp/S102791A002_Barrow_v2.ad2cp (signature 250)", {
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

        expect_equal(sort(names(d[["average"]])),
            c("a", "accelerometerx", "accelerometery", "accelerometerz",
                "blankingDistance", "cellSize", "datasetDescription",
                "ensemble", "heading", "magnetometerx", "magnetometery",
                "magnetometerz", "nominalCorrelation", "numberOfBeams",
                "numberOfCells", "oceCoordinate", "orientation",
                "originalCoordinate", "pitch", "powerLevel", "pressure", "q",
                "roll", "soundSpeed", "temperature", "temperatureMagnetometer",
                "temperatureRTC", "time", "transmitEnergy", "v"))
        # Beams and cells
        expect_equal(d[["oceCoordinate"]], "beam")
        expect_equal(d[["cellSize", "average"]], 1)
        expect_equal(d[["blankingDistance", "average"]], 0.5)
        expect_equal(d[["numberOfBeams", "average"]], 4)
        expect_equal(d[["numberOfCells", "average"]], 87)
        # Some velocity slices
        v <- d[["v"]]
        expect_equal(v[1,,1],
            c(-0.553, 2.255, 2.003, -1.161, 2.121, 1.061, 2.274, 2.231, 0.602,
                2.28, 2.061, -2.174, 0.267, -0.979, 1.874, 2.339, -1.965, 1.31,
                1.297, 2.34, 1.619, 1.846, -2.148, -0.709, 1.776, 2.569, 0.275,
                1.937, 2.469, 2.405, 0.958, 0.608, 0.193, 2.443, 1.023, -1.218,
                -0.328, -0.751, -1.775, -2.386, -2.006, 2.397, 0.432, -1.961,
                -1.587, -1.973, -0.209, 0.541, -2.069, -2.571, 0.647, 0.406,
                -0.197, 1.148, -2.386, 0.4, -1.223, -1.118, -1.235, -0.055, 0.649,
                1.906, 2.149, -0.186, 1.775, 2.362, 2.104, -0.517, -2.445, -1.806,
                2.138, -2.557, 1.898, 1.279, 1.778, 0.371, 0.886, 0.814, -0.86,
                -2.291, 1.665, -0.911, -1.491, -1.318, 0.396, 1.649, -0.927))
        expect_equal(v[1,1,],
            c(-0.553, 0.647, 0.099, 0.045))
        expect_equal(v[1,,2],
            c(0.647, 1.701, 0.861, 1.612, 2.451, 2.157, -0.347, 0.131, 1.487,
                -0.937, -0.98, 1.664, 2.114, -2.224, -2.434, 2.004, 1.927,
                1.105, 1.34, -2.118, -2.236, -0.206, 1.583, -1.877, 0.846,
                0.672, -1.246, -0.162, -2.063, -2.069, -0.158, 1.725, 0.688,
                -0.789, -0.092, 1.385, -1.389, 0.318, -0.334, 0.433, -2.564,
                2.014, 2.355, -1.102, 0.829, 2.156, 2.599, -1.577, -0.562,
                0.001, 2.088, 0.484, -1.586, 1.221, 0.645, -2.272, 1.802, 2.563,
                0.654, -1.036, -0.618, -0.333, -0.73, -0.021, 1.204, 0.059,
                2.475, 1.655, -2.077, -2.062, -1.222, -0.958, 1.279, -1.897,
                -0.208, 1.232, 1.185, -1.423, -2.429, 0.218, 2.204, 1.773,
                -1.272, -0.794, -1.35, 1.464, -1.952))

        # burstAltimeterRaw
        bar <- d[["burstAltimeterRaw"]]
        expect_equal("beam", bar$originalCoordinate)
        expect_equal("beam", bar$oceCoordinate)
        # FIXME: ensemble not checked (and it's always 1, which *must* be wrong)
        expect_equal(head(bar[["magnetometerx"]]),
            c(356L, 354L, 355L, 355L, 354L, 355L))
        expect_equal(head(bar[["magnetometery"]]),
            c(-310L, -310L, -310L, -311L, -309L, -310L))
        expect_equal(head(bar[["magnetometerz"]]),
            c(171L, 171L, 170L, 170L, 169L, 170L))
        expect_equal(head(bar[["accelerometerx"]]),
            c(-0.71142578125, -0.71142578125, -0.7119140625, -0.711669921875,
                -0.71142578125, -0.711669921875))
        expect_equal(head(bar[["accelerometery"]]),
            c(0.70111083984375, 0.70086669921875, 0.70086669921875,
                0.70086669921875, 0.70111083984375, 0.70062255859375))
        expect_equal(head(bar[["accelerometerz"]]),
            c(0.01654052734375, 0.01654052734375, 0.01654052734375,
                0.01654052734375, 0.01654052734375, 0.01678466796875))
        expect_equal(1833L, bar$altimeterRawNumberOfSamples)
        expect_equal(bar$altimeterRawSamples[1:3,1:3],
            structure(c(2313L, 1542L, 2560L, 2560L, 2442L, 3150L, 2313L, 4570L,
                    3579L), dim = c(3L, 3L)))
        expect_equal(head(bar$altimeterDistance),
            c(42.0308685302734, 42.1037330627441, 42.1036491394043, 40.7197113037109,
                42.1040687561035, 40.7203636169434))
        expect_equal(head(bar$pressure),
            c(0.807, 0.801, 0.8, 0.792, 0.808, 0.796))
        expect_true(all("zup" == bar[["orientation"]]))
        expect_equal(head(bar$ASTDistance),
            c(42.1686553955078, 42.2458877563477, 42.2420387268066,
                40.852237701416, 42.2431640625, 40.8575706481934))
        expect_equal(head(bar$ASTPressure),
            c(0.806999981403351, 0.800999999046326, 0.800000011920929,
                0.791999995708466, 0.808000028133392, 0.796000003814697))

        # Time
        expect_equal(head(d[["time"]]),
            structure(c(1653479685.6677, 1653479687.6677, 1653479689.6677,
                    1653479691.6677, 1653479693.6677, 1653479695.6677), class =
                c("POSIXct", "POSIXt"), tzone = "UTC"))
        expect_equal(d[["time"]], d[["time", "average"]])

        # Pressure, temperature etc
        expect_equal(head(d[["pressure"]]),
            c(0.791, 0.801, 0.807, 0.796, 0.803, 0.799))
        expect_equal(d[["pressure", "average"]], d[["pressure"]])
        expect_equal(head(d[["temperature"]]),
            c(20.18, 20.18, 20.18, 20.18, 20.18, 20.18))
        expect_equal(d[["temperature"]], d[["temperature", "average"]])
        expect_equal(head(d[["heading"]]),
            c(280.21, 279.75, 279.97, 279.75, 279.72, 279.82))
        expect_equal(d[["heading"]], d[["heading", "average"]])
        expect_equal(head(d[["pitch"]]),
            c(-45.4, -45.43, -45.43, -45.41, -45.42, -45.42))
        expect_equal(d[["pitch"]], d[["pitch", "average"]])

        expect_equal(head(d[["roll"]]),
            c(88.64, 88.62, 88.62, 88.64, 88.64, 88.62))
        expect_equal(d[["roll"]], d[["roll", "average"]])
})
}

