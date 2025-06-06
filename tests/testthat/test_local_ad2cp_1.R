# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)

# The files used here are in the possession of Dan Kelley, but cannot
# be shared with anyone else, since they were provided to him in
# confidence, to help with testing.
#
# NOTES
#
# 1. The first test file (creating object d1) is checked against results
# from a Matlab non-public script, as well as against some values inferred
# from the header.  There is a fair amount of guessing on tests against
# the header, since the main code of read.adp.ad2cp() focusses on the
# binary data, and so most of the study, so far, has been of the
# documentation for those data.
#
# 2. blankingDistance is tricky, and requires discussion.  In the next numbered
# paragraph are my notes prior to 2022-08-29.  But on that date, I learned
# that the Nortek software has a bug for echosounder type (0x1c), in that the
# value of what I call blankingDistanceInCm is *incorrect*, and is (I think)
# always FALSE.  I guess the software will change at some point but then
# old files will still be wrong.  I decided to just take Nortek at its word
# and assume that echosounder=0x1c will *always* report blankingDistance
# in mm.  Still, I am retaining the next paragraph because it might be useful.
#
#' 3. (read 2 first!) Note that the files cover 2 cases of blankingDistance, which
# can be in cm units in the file, or in mm units. The mm unit had to be
# inferred by inspection of the file headers, since the Nortek
# documentation is not clear; reference 1 table 6.1.2 on page 49
# suggests blankingDistance is in cm if status[2,] (that is, bit 1 in
# the Nortek count-from-0 notation) is 0x01, but there is no indication of the
# unit used, if status[2,] is 0x00, and the assumption of mm in that
# case is based on examination of header information. To see the bits
# in status[2,], use read.adp.ad2cp(..., debug=1). UPDATE: 2022-07-22
# I am trying to use this status bit but something is just plain wrong.
# The file has 2000 stored, so assuming cm gives 20m, which seems wrong
# and also the header says 2.000 for BD. Since Nortek (2022) has multiple
# errors, I am going to go with what BD says, and I will assume that
# the 1 means in mm, not in cm.
#
# REFERENCES
# 1. Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS, 2017.

f1 <- "~/Dropbox/oce_secret_data/ad2cp/secret1_trimmed.ad2cp"
f2 <- "~/Dropbox/oce_secret_data/ad2cp/secret2_trimmed.ad2cp"
f3 <- "~/Dropbox/oce_secret_data/ad2cp/secret3_trimmed.ad2cp"
# f4 <- "~/Dropbox/oce_secret_data/ad2cp/byg_trimmed.ad2cp"
# f5 <- "~/Dropbox/oce_secret_data/ad2cp/med_trimmed.ad2cp"
#   > read.oce(f1)
#     IDhex IDdec    name occurance
#   1  0x15    21   burst        88
#   2  0x16    22 average        11
#   3  0xa0   160    text         1
#
#   > read.oce(f2)
#     IDhex IDdec  name occurance
#   1  0x15    21 burst        99
#   2  0xa0   160  text         1
#
#   > read.oce(f3)
#     IDhex IDdec             name occurance
#   1  0x15    21            burst        50
#   2  0x18    24 interleavedBurst        49
#   3  0xa0   160             text         1


if (file.exists(f1)) {
    test_that("'dataType' works for 0x16, 24, and \"average\"", {
        expect_message(
            d1 <- read.oce(f1, dataType = "average"),
            "setting plan=0"
        )
        expect_equal(11L, length(d1[["time"]]))
        expect_message(
            d2 <- read.oce(f1, dataType = 22),
            "setting plan=0"
        )
        expect_message(
            d3 <- read.oce(f1, dataType = "average"),
            "setting plan=0"
        )
        expect_equal(d1@data, d2@data)
        expect_equal(d1@data, d3@data)
    })

    test_that("read.adp.ad2cp() on a private AD2CP file that has 'average' and 'burst' data", {
        # FIXME: handle 0x15
        average <- read.adp.ad2cp(f1, 1, 100, 1, dataType = "average", plan = 0)
        burst <- read.adp.ad2cp(f1, 1, 100, 1, dataType = "burst", plan = 0)
        expect_error(
            read.adp.ad2cp(f1, 1, 100, 1, dataType = "average", plan = 10),
            "there are no data for plan=10; try one of the following values instead: 1 0"
        )
        expect_message(
            d1 <- read.adp.ad2cp(f1, 1, 100, 1, dataType = "average"),
            "setting plan=0, the most common value in this file"
        )
        # regression tests
        expect_equal(
            names(average[["data"]]),
            c(
                "nominalCorrelation", "ensemble", "time", "soundSpeed", "temperature",
                "pressure", "heading", "pitch", "roll", "magnetometer", "accelerometer",
                "temperatureMagnetometer", "temperatureRTC", "transmitEnergy",
                "powerLevel", "distance", "v", "a", "q", "AHRSRotationMatrix",
                "AHRSQuaternionsW", "AHRSQuaternionsX", "AHRSQuaternionsY", "AHRSQuaternionsZ",
                "AHRSGyroX", "AHRSGyroY", "AHRSGyroZ"
            )
        )
        expect_equal(
            names(burst[["data"]]),
            c(
                "nominalCorrelation", "ensemble", "time", "soundSpeed", "temperature",
                "pressure", "heading", "pitch", "roll", "magnetometer", "accelerometer",
                "temperatureMagnetometer", "temperatureRTC", "transmitEnergy",
                "powerLevel", "distance", "v", "a", "q", "AHRSRotationMatrix",
                "AHRSQuaternionsW", "AHRSQuaternionsX", "AHRSQuaternionsY", "AHRSQuaternionsZ",
                "AHRSGyroX", "AHRSGyroY", "AHRSGyroZ"
            )
        )
        expect_equal(average[["type"]], "Signature1000")
        expect_equal(average[["type"]], ad2cpHeaderValue(d1, "ID", "STR", FALSE))
        expect_equal(average[["fileType"]], "AD2CP")
        expect_equal(average[["serialNumber"]], ad2cpHeaderValue(d1, "ID", "SN"))
        expect_equal(burst[["type"]], "Signature1000")
        expect_equal(burst[["type"]], ad2cpHeaderValue(d1, "ID", "STR", FALSE))
        expect_equal(burst[["fileType"]], "AD2CP")
        expect_equal(burst[["serialNumber"]], ad2cpHeaderValue(d1, "ID", "SN"))
        expect_equal(burst[["oceCoordinate"]], "beam")
        expect_equal(average[["cellSize"]], ad2cpHeaderValue(d1, "GETAVG", "CS", plan = 0))
        expect_equal(average[["blankingDistance"]], ad2cpHeaderValue(d1, "GETAVG", "BD"))
        expect_equal(average[["oceCoordinate"]], tolower(ad2cpHeaderValue(d1, "GETAVG", "CY", FALSE)))
        expect_equal(burst[["cellSize"]], ad2cpHeaderValue(d1, "GETBURSTHR", "CS"))
        expect_equal(burst[["blankingDistance"]], ad2cpHeaderValue(d1, "GETBURSTHR", "BD"))
        # FIXME: the next uses GETBURST, not GETBURSTHR. I do not understand the format
        expect_equal(burst[["oceCoordinate"]], tolower(ad2cpHeaderValue(d1, "GETBURST", "CY", FALSE)))
        # FIXME: the next tests will fail if we store AHRS as 3D array
        # >> Data.BurstHR_AHRSRotationMatrix(1,:)
        expect_equal(
            burst[["AHRSRotationMatrix"]][1, , ],
            matrix(c(
                0.060653746, -0.37823972, -0.92368418, 0.31505784,
                -0.87079191, 0.37727141, -0.94709891, -0.31389475,
                0.066413939
            ), byrow = TRUE, nrow = 3)
        ) # byrow because numbers are from matlab output
        # >> load labtestsig3.ad2cp.00000_1.mat
        expect_equal(burst[["numberOfBeams"]], 1)
        expect_equal(burst[["numberOfBeams"]], ad2cpHeaderValue(d1, "GETBURST", "NB"))
        expect_equal(burst[["numberOfCells"]], 256)
        expect_equal(burst[["numberOfCells"]], ad2cpHeaderValue(d1, "GETBURSTHR", "NC"))
        expect_equal(average[["numberOfBeams"]], 4)
        expect_equal(average[["numberOfBeams"]], ad2cpHeaderValue(d1, "GETAVG1", "NB"))
        expect_equal(average[["numberOfCells"]], 150)
        expect_equal(average[["numberOfCells"]], ad2cpHeaderValue(d1, "GETAVG1", "NC"))
        # > Data.BurstHR_EnsembleCount(1:10)
        ensembleMatlab <- c(
            969, 970, 971, 972, 973,
            974, 975, 976, 977, 978
        )
        expect_equal(burst[["ensemble"]][1:10], ensembleMatlab)
        #> Data.Average_EnsembleCount
        expect_true(all(1 == average[["ensemble"]]))
        # timestamps
        # >> format long
        # >> Data.Average_TimeStamp(1:10)
        secAverageMatlab <- 1e9 * c(
            1.490564521063300, 1.490564522063300, 1.490564523063300,
            1.490564524063300, 1.490564525063300, 1.490564526063300,
            1.490564527063300, 1.490564528063300, 1.490564529063300,
            1.490564530063300
        )
        timeAverageMatlab <- numberAsPOSIXct(secAverageMatlab)
        expect_equal(average[["time"]][1:10], timeAverageMatlab)
        # >> format long
        # >> Data.BurstHR_TimeStamp(1:10)
        secBurstMatlab <- 1e9 * c(
            1.490564521001000, 1.490564521125800, 1.490564521251000,
            1.490564521376000, 1.490564521501000, 1.490564521626000,
            1.490564521751000, 1.490564521876000, 1.490564522001000,
            1.490564522125800
        )
        timeBurstMatlab <- numberAsPOSIXct(secBurstMatlab)
        expect_equal(burst[["time"]][1:10], timeBurstMatlab)
        #>> Data.BurstHR_TransmitEnergy(1:10)
        expect_equal(burst[["transmitEnergy"]][1:10], c(4, 0, 4, 4, 4, 4, 4, 4, 4, 0))
        #>> Data.Average_TransmitEnergy(1:10)
        expect_equal(average[["transmitEnergy"]][1:10], c(82, 82, 82, 82, 82, 82, 82, 82, 82, 82))

        # >> Data.Average_Pressure(1:10)
        pressureAverageMatlab <- c(
            10.259, 10.260, 10.262, 10.262, 10.258,
            10.260, 10.261, 10.256, 10.259, 10.261
        )
        expect_equal(average[["pressure"]][1:10], pressureAverageMatlab)
        # >> Data.BurstHR_Pressure(1:10)
        pressureBurstMatlab <- c(
            10.260, 10.258, 10.264, 10.261, 10.263,
            10.260, 10.260, 10.261, 10.259, 10.259
        )
        expect_equal(burst[["pressure"]][1:10], pressureBurstMatlab)
        # >> Data.BurstHR_WaterTemperature(1:10)
        temperatureMatlab <- c(
            24.010, 24.000, 24.010, 24.010, 24.010,
            24.010, 24.010, 24.010, 24.010, 24.000
        )
        expect_equal(burst[["temperature"]][1:10], temperatureMatlab)
        # > Data.AverageHR_MagnetometerTemperature(1:10)
        temperatureMagAvgMatlab <- c(
            25.8920, 25.8920, 25.8920, 25.8450, 25.8920,
            25.8450, 25.8920, 25.8450, 25.8920, 25.8450
        )
        expect_equal(average[["temperatureMagnetometer"]][1:10], temperatureMagAvgMatlab)
        # > Data.AverageHR_RTCTemperature(1:10)
        temperatureRTCAverageMatlab <- c(
            28.5000, 28.5000, 28.7500, 28.7500, 28.7500,
            28.7500, 28.7500, 28.7500, 28.7500, 28.7500
        )
        expect_equal(average[["temperatureRTC"]][1:10], temperatureRTCAverageMatlab)
        # > Data.BurstHR_MagnetometerTemperature(1:10)
        temperatureMagBurstMatlab <- c(
            25.7980, 25.8450, 25.9390, 25.8920, 25.8450,
            25.7510, 25.7980, 25.8920, 25.8450, 25.7980
        )
        expect_equal(burst[["temperatureMagnetometer"]][1:10], temperatureMagBurstMatlab)
        # > Data.BurstHR_RTCTemperature(1:10)
        temperatureRTCBurstMatlab <- c(
            28.500, 28.500, 28.500, 28.500, 28.500,
            28.500, 28.500, 28.500, 28.500, 28.500
        )
        expect_equal(burst[["temperatureRTC"]][1:10], temperatureRTCBurstMatlab)
        # >> Data.BurstHR_Heading(1:10)
        headingMatlab <- c(
            10.890, 10.910, 10.920, 10.980, 10.960,
            10.910, 10.900, 10.900, 10.900, 10.900
        )
        expect_equal(burst[["heading"]][1:10], headingMatlab)
        # >> Data.BurstHR_Pitch(1:10)
        pitchMatlab <- c(
            -71.280, -71.280, -71.270, -71.280, -71.280,
            -71.280, -71.270, -71.270, -71.270, -71.270
        )
        expect_equal(burst[["pitch"]][1:10], pitchMatlab)
        # >> Data.BurstHR_ROll(1:10)
        rollMatlab <- c(
            -78.050, -78.080, -78.080, -78.090, -78.090,
            -78.080, -78.080, -78.080, -78.080, -78.080
        )
        expect_equal(burst[["roll"]][1:10], rollMatlab)
        # >> Data.Average_CellSize
        # ans = single 0.2000
        expect_equal(average[["cellSize"]], 0.2)
        # >> Data.BurstHR_CellSize(1)
        # ans = single 0.0200
        expect_equal(burst[["cellSize"]], 0.02)
        # >> Data.Alt_Average_CellSize(1)
        # ans = single 0.2000
        # >> Data.Alt_BurstHR_CellSize(1)
        # ans = single 0.0500
        # >> Data.Average_Blanking(1)
        # ans = 0.1000
        expect_equal(average[["blankingDistance"]], 0.1)
        # >> Data.BurstHR_Blanking(1)
        # ans = 2.8000
        expect_equal(burst[["blankingDistance"]], 2.8)
        # >> Data.Alt_BurstHR_Blanking(1)
        # ans = 0.1000
        # >> Data.Alt_Average_Blanking(1)
        # ans = 0.1000
        beam2xyzAverageMatlab <- matrix(
            c(
                1.1831000, 0.00000000, -1.1831000, 0.0000000,
                0.0000000, -1.1831000, 0.0000000, 1.1831000,
                0.5518000, 0.00000000, 0.5518000, 0.0000000,
                0.0000000, 0.5518000, 0.0000000, 0.5518000
            ),
            nrow = 4, byrow = TRUE
        )
        # FIXME: check beam2xyzAverageMatlab
        # print(beam2xyzAverageMatlab)
        # >> Data.BurstHR_NominalCor(1:10)
        nominalCorrelationBurstMatlab <- rep(100, 10)
        # R > d1[["nomcor", "burst"]][1:10]
        # R [1]  49 100  33 100 100 100 100 100 100 100
        expect_equal(burst[["nominalCorrelation"]][1:10], nominalCorrelationBurstMatlab)
        #>> Data.Average_NominalCor(1:10)
        nominalCorrAvgMatlab <- rep(33, 10)
        expect_equal(average[["nominalCorrelation"]][1:10], nominalCorrAvgMatlab)
        # All are zero in matlab.
        expect_true(all(0 == average[["powerLevel"]]))
        expect_true(all(0 == burst[["powerLevel"]]))
        # relax tolerance since it's a 16-bit value
        # >> Data.Average_AccelerometerX(1:10)
        accxAverageMatlab <- c(
            -0.9497070, -0.9492188, -0.9477539, -0.9472656, -0.9458008,
            -0.9497070, -0.9501953, -0.9516602, -0.9511719, -0.9516602
        )
        accel <- average[["accelerometer"]]
        expect_equal(accel[1:10, 1], accxAverageMatlab, tolerance = 1e-5)
        # >> Data.Average_AccelerometerY(1:10)
        accyAverageMatlab <- c(
            -0.3134766, -0.3139648, -0.3168945, -0.3125000, -0.3178711,
            -0.3164062, -0.3168945, -0.3129883, -0.3154297, -0.3154297
        )
        expect_equal(accel[1:10, 2], accyAverageMatlab, tolerance = 1e-5)
        # >> Data.Average_AccelerometerZ(1:10)
        acczAverageMatlab <- c(
            0.0668945, 0.0649414, 0.0659180, 0.0649414, 0.0678711,
            0.0668945, 0.0693359, 0.0693359, 0.0649414, 0.0649414
        )
        expect_equal(accel[1:10, 3], acczAverageMatlab, tolerance = 1e-5)
        #>> Data.BurstHR_AccelerometerX(1:10)
        accxBurstMatlab <- c(
            -0.9472656, -0.9497070, -0.9492188, -0.9467773, -0.9511719,
            -0.9506836, -0.9472656, -0.9492188, -0.9482422, -0.9506836
        )

        expect_equal(burst[["accelerometer"]][1:10, 1], accxBurstMatlab, tolerance = 1e-5)
        #>> Data.BurstHR_AccelerometerY(1:10)
        accyBurstMatlab <- c(
            -0.3144531, -0.3178711, -0.3159180, -0.3168945, -0.3149414,
            -0.3154297, -0.3168945, -0.3139648, -0.3183594, -0.3154297
        )
        expect_equal(burst[["accelerometer"]][1:10, 2], accyBurstMatlab, tolerance = 1e-5)
        #>> Data.BurstHR_AccelerometerZ(1:10)
        acczBurstMatlab <- c(
            0.066895, 0.065918, 0.065430, 0.066406, 0.065918,
            0.068359, 0.070801, 0.068359, 0.069336, 0.069336
        )
        expect_equal(burst[["accelerometer"]][1:10, 3], acczBurstMatlab, tolerance = 1e-5)
        expect_true(is.null(average[["junk"]]))
        expect_equal(dim(average[["v"]]), c(11, 150, 4))
        expect_equal(dim(burst[["v"]]), c(88, 256, 1))
        # >> Data.BurstHR_VelBeam5(1,1:10)
        # Note that bursts store in beam 5. FIXME: next broken 2022-07-13
        expect_equal(burst[["v"]][1, 1:4, 1], c(0.36240, 0.35830, 0.36430, 0.20590))
        # >> Data.Average_VelBeam1(1,1:4)
        expect_equal(average[["v"]][1, 1:4, 1], c(-0.8170, -0.8890, -1.9170, -2.1110))
        #>> Data.Average_VelBeam1(2,1:4)
        expect_equal(average[["v"]][2, 1:4, 1], c(-0.7800, -2.3230, -1.0840, -0.8010))
        # >> Data.Average_VelBeam2(1,1:4)
        expect_equal(average[["v"]][1, 1:4, 2], c(-0.1630, 1.6930, 1.8490, 1.1120))
        #>> Data.Average_VelBeam2(2,1:4)
        expect_equal(average[["v"]][2, 1:4, 2], c(-0.6340, 1.4590, 1.9590, 0.9400))
        #>> Data.Average_VelBeam3(1,1:4)
        expect_equal(average[["v"]][1, 1:4, 3], c(-1.5600, 1.4140, 1.5630, 1.5510))
        #>> Data.BurstHR_AmpBeam5(1:4,1,1)
        expect_equal(0.5 * as.numeric(burst[["a"]][1:4, 1, 1]), c(34.0, 34.0, 36.0, 34.0))
        # >> Data.BurstHR_AmpBeam5(1:4,2,1)
        expect_equal(0.5 * as.numeric(burst[["a"]][1:4, 2, 1]), c(34.5000, 35.0000, 36.5000, 35.0000))
        # >> Data.BurstHR_AmpBeam5(1:4,3,1)
        expect_equal(0.5 * as.numeric(burst[["a"]][1:4, 3, 1]), c(34.5000, 35.5000, 37.5000, 34.0000))
        # >> Data.BurstHR_AmpBeam5(1:4,4,1)
        expect_equal(0.5 * as.numeric(burst[["a"]][1:4, 4, 1]), c(35.0000, 37.5000, 36.0000, 36.5000))
        #>> Data.Average_AmpBeam1(1:4,1,1)
        expect_equal(0.5 * as.numeric(average[["a"]][1:4, 1, 1]), c(43.5000, 42.5000, 44.0000, 43.0000))
        # expect_equal(0.5 * d1[["a","average numeric"]][1:4, 1, 1], c(43.5000, 42.5000, 44.0000, 43.0000))
        #>> Data.Average_AmpBeam1(1:4,2,1)
        # expect_equal(0.5 * d1[["a","average numeric"]][1:4, 2, 1], c(40.0000, 40.0000, 39.5000, 39.5000))
        #>> Data.Average_AmpBeam1(1:4,3,1)
        # expect_equal(0.5 * d1[["a","average numeric"]][1:4, 3, 1], c(39.5000, 40.0000, 39.5000, 40.0000))
        #>> Data.BurstHR_CorBeam5(1:4,1)
        expect_equal(as.numeric(burst[["q"]][1:4, 1, 1]), c(72, 81, 63, 78))
        #>> Data.BurstHR_CorBeam5(1:4,2)
        expect_equal(as.numeric(burst[["q"]][1:4, 2, 1]), c(76, 49, 89, 79))
        #>> Data.BurstHR_CorBeam5(1:4,3)
        # expect_equal(d1[["q", "burst numeric"]][1:4, 3, 1], c(91, 76, 70, 72))
        #>> Data.Average_CorBeam1(1:4,1)
        # expect_equal(d1[["q", "numeric"]][1:4, 1, 1], c(49, 44, 36, 44 ))
        # expect_equal(d1[["q", "average numeric"]][1:4, 1, 1], c(49, 44, 36, 44))
        #>> Data.Average_CorBeam1(1:4,2)
        # expect_equal(d1[["q", "average numeric"]][1:4, 2, 1], c( 7,  4,  5,  3))
        #>> Data.Average_CorBeam2(1:4,1)
        # expect_equal(d1[["q", "average numeric"]][1:4, 1, 2], c(19,  3,  3,  3))
        #>> Data.Average_CorBeam3(1:4,1)
        # expect_equal(d1[["q", "average numeric"]][1:4, 1, 3], c( 4, 11,  8, 15))
        #>> Data.Average_CorBeam4(1:4,1)
        # expect_equal(d1[["q", "average numeric"]][1:4, 1, 4], c(38, 53, 71, 66))
        # Next is not a test against matlab ground-truth, since I don't
        # have any ENU from matlab, but rather it is just a test against
        # code changes, with the check values being what the code
        # produced on 2019-01-06.
        d1enu <- toEnu(d1)
        # FIXME: add a way to do coordinate transformations
        # later expect_equal(d1enu[["average"]]$v[1:2,1:2,1:4],
        # later     structure(c(-0.0423407864127893, -0.0412083140396359,
        # later             -1.18658233863229, -0.655501998134796, -2.04226357900539,
        # later             -2.20814086810278, -0.684468639431669, -1.64208610236321,
        # later             1.34600003150129, 0.905301482665873, 3.00993173312586,
        # later             4.39764211472632, 0.551305292490936, 0.155603618214437,
        # later             -1.78358555931817, -2.23533799929557), dim = c(2L, 2L, 4L)))
        # expect_silent(plot(d1))
        # expect_silent(plot(d1, which="velocity"))
        # expect_silent(plot(d1, which="amplitude"))
        # expect_silent(plot(d1, which="quality"))
        # expect_silent(plot(d1, which="hydrography"))
        # expect_silent(plot(d1, which="angles"))
        # expect_silent(plot(d1, which="uv"))
        # expect_silent(plot(d1, which="uv+ellipse"))
        # expect_silent(plot(d1, which="uv+ellipse+arrow"))
        # expect_error(plot(d1, which="bottomRange"), "ADP object lacks bottom-tracking data")
        # expect_silent(plot(d1, which="progressiveVector"))
    })
}

if (file.exists(f2)) {
    test_that("read.adp() on a private AD2CP file that has only 'burst' data", {
        N <- 99 # known value for subset of a larger file
        expect_silent(toc <- read.oce(f2, TOC = TRUE))
        expect_equal(N, toc$count[1])
        # Note: using read.adp() to ensure that it also works
        expect_warning(
            expect_warning(
                expect_message(
                    burst <- read.adp(f2, dataType = "burst"),
                    "setting plan=0, the only value in the file"
                ),
                "ignoring 'despike'"
            ),
            "ignoring 'monitor'"
        )
        expect_equal(burst[["oceCoordinate"]], "beam")
        expect_equal(
            names(burst@data),
            c(
                "nominalCorrelation", "ensemble", "time", "soundSpeed",
                "temperature", "pressure", "heading", "pitch", "roll",
                "magnetometer", "accelerometer",
                "temperatureMagnetometer", "temperatureRTC",
                "transmitEnergy", "powerLevel", "v", "a", "q"
            )
        )
        expect_equal(burst[["fileType"]], "AD2CP")
        expect_equal(burst[["serialNumber"]], ad2cpHeaderValue(burst, "ID", "SN"))
        expect_equal(burst[["type"]], "Aquadopp2")
        expect_equal(burst[["type"]], ad2cpHeaderValue(burst, "ID", "STR", FALSE))
        expect_equal(burst[["cellSize"]], 0.2)
        expect_equal(burst[["cellSize"]], ad2cpHeaderValue(burst, "GETBURST", "CS"))
        expect_equal(burst[["blankingDistance"]], ad2cpHeaderValue(burst, "GETBURST", "BD"))
        expect_equal(burst[["blankingDistance"]], ad2cpHeaderValue(burst, "GETBURST", "BD"))
        expect_equal(burst[["oceCoordinate"]], tolower(ad2cpHeaderValue(burst, "GETBURST", "CY", FALSE)))
        expect_true(is.numeric(burst[["v"]]))
        # expect_error(d2[["v", "average"]], "ad2cp object does not contain data item 'average'")
        expect_equal(dim(burst[["v"]]), c(N, 1, 4))
        expect_true(is.raw(burst[["a"]]))
    })
}

if (file.exists(f3)) {
    test_that("read.oce() on a private AD2CP file that has 'burst' and 'interleavedBurst' data", {
        N <- 100
        # Note: using read.oce() to ensure that it also works
        expect_message(
            b <- read.oce(f3, dataType = "burst"),
            "setting plan=1, the only value in the file"
        )
        expect_message(
            ib <- read.oce(f3, dataType = "interleavedBurst"),
            "setting plan=1, the only value in the file"
        )
        # Compare with some header values
        expect_equal(b[["fileType"]], "AD2CP")
        expect_equal(b[["serialNumber"]], ad2cpHeaderValue(b, "ID", "SN"))
        expect_equal("Signature1000", ad2cpHeaderValue(b, "ID", "STR", FALSE))
        expect_equal(b[["type"]], ad2cpHeaderValue(b, "ID", "STR", FALSE))
        expect_equal(b[["cellSize"]], ad2cpHeaderValue(b, "GETBURST", "CS"))
        expect_equal(b[["blankingDistance"]], ad2cpHeaderValue(b, "GETBURST", "BD"))
        # NOTE: the next uses GETBURST, not GETBURSTHR. I do not understand the format
        expect_equal(b[["oceCoordinate"]], tolower(ad2cpHeaderValue(b, "GETBURST", "CY", FALSE)))
        # NB in the header is 5, which I suppose refers to the whole
        # instrument, but d3[["numberOfBeams"]] is 4 for the slant-beam
        # samples and 1 for the vertical-beam samples. But this is an
        # interleavedBurst mode, so I guess 5 is the right number. In any
        # case, I have sidestepped the test.
        #> if (FALSE)
        #>    expect_equal(d3[["burst"]]$numberOfBeams, ad2cpHeaderValue(d3, "GETBURST", "NB"))
        expect_equal(ib[["cellSize"]], ad2cpHeaderValue(ib, "GETBURST1", "CS"))
        expect_equal(ib[["blankingDistance"]], ad2cpHeaderValue(ib, "GETBURST1", "BD"))
        expect_equal(ib[["oceCoordinate"]], tolower(ad2cpHeaderValue(ib, "GETBURST1", "CY", FALSE)))

        # FIXME: I think the nbeams might be wrong for burst

        # The dimension tests are not ground-truthed; they merely reflect
        # what the oce code gives, so that a flag will go off if things
        # change greatly in the code. This also checks the accessors
        # against direct lookup.
        vb <- b[["v"]]
        vib <- ib[["v"]]
        expect_equal(dim(vb), c(50, 88, 4))
        expect_equal(dim(vib), c(49, 88, 1))
        ab <- b[["a"]]
        aib <- ib[["a"]]
        expect_equal(dim(ab), c(50, 88, 4))
        expect_equal(dim(aib), c(49, 88, 1))
        qb <- b[["q"]]
        qib <- ib[["q"]]
        expect_equal(dim(qb), c(50, 88, 4))
        expect_equal(dim(qib), c(49, 88, 1))

        # Note that the interleavedBurst j has just one beam, which
        # I'm guessing might be the vertical one, but that's just a guess,
        # because the nortek system integrator doc has the string
        # 'interleave' on just a single page!
        par(mfcol = c(4, 2))
        zlim <- c(-3, 3)
        tb <- b[["time"]]
        tib <- ib[["time"]]
        db <- b[["distance"]]
        dib <- ib[["distance"]]
        imagep(tb, db, vb[, , 1], zlim = zlim, zlab = "burst[[\"v\"]][,,1]", drawTimeRange = FALSE, ylab = "Distance [m]")
        imagep(tb, db, vb[, , 2], zlim = zlim, zlab = "burst[[\"v\"]][,,2]", drawTimeRange = FALSE, ylab = "Distance [m]")
        imagep(tb, db, vb[, , 3], zlim = zlim, zlab = "burst[[\"v\"]][,,3]", drawTimeRange = FALSE, ylab = "Distance [m]")
        imagep(tb, db, vb[, , 4], zlim = zlim, zlab = "burst[[\"v\"]][,,4]", drawTimeRange = FALSE, ylab = "Distance [m]")
        imagep(tib, dib, vib[, , 1], zlim = zlim, zlab = "interleavedBurst[[\"v\"]][,,1]", drawTimeRange = FALSE, ylab = "Distance [m]")
        plot(c(0, 1), c(0, 1), xlab = "", ylab = "", axes = FALSE, type = "n")
        text(0.5, 0.5, "interleavedBurst has only 1 beam")
        plot(c(0, 1), c(0, 1), xlab = "", ylab = "", axes = FALSE, type = "n")
        text(0.5, 0.5, paste("time range:\n", min(tb), "\n to\n ", max(tb)))

        # below is how we know this is 'beam'
        ## FIXME: look in text for other things, e.g. beam cells etc
        ## t <- d3[["text"]]$text[[1]]
        ## t[grep('CY=',t)[1]]
        # expect_equal("beam", d3[["oceCoordinate"]])
        # expect_error(plot(d3, j="average"), "ad2cp object does not contain data item 'average'")
        # expect_silent(plot(d3))
        # expect_silent(plot(d3, j="burst")) # as above, since object holds 'average' data
        # expect_silent(plot(d3, which="velocity")) # as above, since which='velocity' is default
        # expect_silent(plot(d3, which="amplitude"))
        # expect_silent(plot(d3, which="quality"))
        # expect_silent(plot(d3, j="burst"))
        # expect_silent(plot(d3, which="velocity", j="burst"))
        # expect_silent(plot(d3, which="amplitude", j="burst"))
        # expect_silent(plot(d3, which="quality", j="burst"))
        # par(mfrow=c(1, 1)) # use single-panel since interleavedBurst is just 1 beam
        # expect_silent(plot(d3, j="interleavedBurst"))
        # expect_silent(plot(d3, which="velocity", j="interleavedBurst")) # as above
        # expect_silent(plot(d3, which="amplitude", j="interleavedBurst"))
        # expect_silent(plot(d3, which="quality", j="interleavedBurst"))
        ## Compare beams in three coordinate systems, with 6 plots over two pages
        # par(mfcol=c(3, 2))
        # zlim <- c(-2, 2)
        # expect_silent(plot(d3, which=1, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3, which=2, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3, which=3, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3, which='a1', zlim=c(0, 255), drawTimeRange=FALSE))
        # expect_silent(plot(d3, which='a2', zlim=c(0, 255), drawTimeRange=FALSE))
        # expect_silent(plot(d3, which='a3', zlim=c(0, 255), drawTimeRange=FALSE))
        xyz <- beamToXyz(b)
        # expect_silent(plot(d3xyz, which=1, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3xyz, which=2, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3xyz, which=3, zlim=zlim/4, drawTimeRange=FALSE))
        enu <- xyzToEnu(xyz)
        # expect_silent(plot(d3enu, which=1, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3enu, which=2, zlim=zlim, drawTimeRange=FALSE))
        # expect_silent(plot(d3enu, which=3, zlim=zlim/4, drawTimeRange=FALSE))
    })
}
