# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# This Nortek Signature100 dataset in AD2CP format is an initial fragment of a
# dataset that was kindly donated as a test file in oce by github user
# @krillthor, who helped greatly in the work on github issue
# [1676](https://github.com/dankelley/oce/issues/1676).

# Much of what is done here is to check for consistency with values found on
# 2020-04-15, as a guard against any changes to oce that might alter how it
# reads Nortk ad2cp data.  Several of the values have also been checked (by
# github user @krillthor) against other software that reads ad2cp files.

library(oce)
file <- "local_data/ad2cp/ad2cp_01.ad2cp"
if (file.exists(file)) {
    test_that("local_data/ad2cp/ad2cp_01.ad2cp is okay",
        {
            expect_output(
                expect_warning(
                    expect_warning(
                        expect_warning(
                            d <- read.oce(file),
                            "has 12-byte headers"),
                        "using to=12"),
                    "'plan' defaulting to 0"),
                "EOF before end of chunk")
            # Identifiers
            expect_equal(d[["type"]], "Signature100")
            expect_equal(d[["fileType"]], "AD2CP")
            expect_equal(d[["serialNumber"]], 101135)
            # Entry names
            expect_equal(sort(names(d[["data"]])), c("average", "echosounder", "echosounderRaw"))
            expect_equal(sort(names(d[["average"]])),
                sort(c("a", "accelerometer", "blankingDistance", "cellSize",
                        "configuration", "datasetDescription", "distance",
                        "ensemble", "heading", "magnetometer",
                        "nominalCorrelation", "numberOfBeams", "numberOfCells",
                        "oceCoordinate", "orientation", "originalCoordinate",
                        "pitch", "powerLevel", "pressure", "q", "roll",
                        "soundSpeed", "temperature", "temperatureMagnetometer",
                        "temperatureRTC", "time", "transmitEnergy", "v")))
            # Beams and cells
            expect_equal(d[["average"]]$oceCoordinate, "enu")
            expect_equal(d[["average"]]$cellSize, 10)
            expect_equal(d[["echosounder"]]$cellSize, 0.75)
            expect_equal(d[["average"]]$blankingDistance, 2)
            expect_equal(d[["average"]]$numberOfBeams, 4)
            expect_equal(d[["average"]]$numberOfCells, 32)
            expect_equal(d[["echosounder"]]$numberOfCells, 438)

            # v[1,,1] values from the IMOS toolbox; see
            # https://github.com/dankelley/oce/issues/1975#issuecomment-1180403719)
            v <- d[["average"]]$v
            expect_equal(v[1,,1],
                c(-0.832, -0.048, 0.001, 0.003, 0.027, 0.148, 0.066, -0.067,
                    -0.056, -0.103, -0.07, -0.051, 0.071, 0.062, 0.1, 0.067,
                    0.023, 0.026, -0.119, -0.013, 0.004, -0.094, -0.041, 0.151,
                    0.145, 0.465, 0.462, 0.456, 1.029, 0.342, 0.29, 0.3))
            expect_equal(v[1,1,], c(-0.832, 0.901, -0.013, 0.961))
            # v[1,,2] values from the IMOS toolbox; see
            # https://github.com/dankelley/oce/issues/1975#issuecomment-1180403719)
            expect_equal(v[1,,2],
                c(0.901, -0.022, 0.014, -0.103, -0.114, -0.039, -0.005, -0.065,
                    -0.074, -0.125, -0.301, -0.271, -0.28, -0.046, -0.092,
                    -0.128, -0.229, -0.245, -0.175, -0.171, -0.129, -0.031,
                    -0.166, -0.011, -0.058, -0.451, -0.671, -0.747, -0.812,
                    -0.421, -0.336, -0.489))
            # Time
            expect_equal(d[["average"]]$time,
                structure(c(1559664065.001, 1559664070.0011, 1559664075.001,
                        1559664080.0011, 1559664085.001, 1559664090.001,
                        1559664095.001),
                    class=c("POSIXct", "POSIXt"),
                    tzone="UTC"))

            expect_equal(d[["echosounder"]]$time,
                structure(c(1559664081.001, 1559664101.001),
                    class=c("POSIXct", "POSIXt"),
                    tzone="UTC"))

            # Pressure, temperature etc
            expect_equal(d[["average"]]$pressure,
                c(269.143, 269.144, 269.221, 269.123, 269.187, 269.188, 269.183))
            expect_equal(d[["echosounder"]]$pressure,
                c(269.114, 269.164))

            expect_equal(d[["average"]]$temperature,
                c(1.51, 1.48, 1.5, 1.48, 1.51, 1.49, 1.49))
            expect_equal(d[["echosounder"]]$temperature,
                c(1.51, 1.51))

            expect_equal(d[["average"]]$heading,
                c(153.48, 153.36, 153.04, 152.66, 152.35, 151.77, 151.58))
            expect_equal(d[["echosounder"]]$heading,
                c(152.92, 151.19))

            expect_equal(d[["average"]]$pitch,
                c(-0.25, -0.25, -0.23, -0.24, -0.25, -0.2, -0.24))
            expect_equal(d[["echosounder"]]$pitch,
                c(-0.21, -0.25))

            expect_equal(d[["average"]]$roll,
                c(0, 0, 0.02, 0.02, -0.01, -0.02, 0.03))
            expect_equal(d[["echosounder"]]$roll,
                c(0, 0.04))
        })
}

