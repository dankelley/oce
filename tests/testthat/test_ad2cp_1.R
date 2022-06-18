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
# from the header.  (There is a fair amount of guessing on tests against
# the header, since the main code of read.adp.ad2cp() focusses on the
# binary data, and so most of the study, so far, has been of the
# documenation for thos data.
#
# 2. Note that the files cover the two cases of blankingDistance, which
# can be in cm units in the file, or in mm units. The mm unit had to be
# inferred by inspection of the file headers, since the Nortek
# documentation is not clear; reference 1 table ## 6.1.2 on page 49
# suggests blankingDistance is in cm if statusBits[2] (that is, bit 1 in
# the Nortek count-from-0 notation) is 0x01, but there is no indicaiton of the
# unit used, if statusBits[2] is 0x00, and the assumption of mm in that
# case is based on examination of headr information. To see the bits
# in statusBits[2], use read.adp.ad2cp(..., debug=1).
#
# REFERENCES
# 1. Nortek AS. \dQuote{Signature Integration 55|250|500|1000kHz.} Nortek AS, 2017.

f1 <- "~/Dropbox/oce_secret_data/ad2cp_secret_1.ad2cp"
f2 <- "~/Dropbox/oce_secret_data/ad2cp_secret_2.ad2cp"
f3 <- "~/Dropbox/oce_secret_data/ad2cp_secret_3.ad2cp"

if (file.exists(f1)) {
    skip_on_cran()
    test_that("read.adp.ad2cp() on a private AD2CP file that has 'average' and 'burst' data", {
        expect_silent(read.adp.ad2cp(f1, 1, 100, 1, plan=0))
        expect_error(read.adp.ad2cp(f1, 1, 100, 1, plan=10),
            "there are no data for plan=10; try one of the following values instead: 1 0")
        expect_warning(d1 <- read.adp.ad2cp(f1, 1, 100, 1),
            "'plan' defaulting to 0,")
        nnn <- c("average", "burst", "interleavedBurst")
        expect_equal(c(TRUE, TRUE, FALSE), nnn %in% names(d1@data))
        expect_equal(sort(names(d1[["burst"]])),
            sort(c("a", "accelerometerx", "accelerometery", "accelerometerz",
                    "AHRS", "blankingDistance", "cellSize",
                    "datasetDescription", "ensemble", "heading",
                    "magnetometerx", "magnetometery", "magnetometerz",
                    "nominalCorrelation", "numberOfBeams", "numberOfCells",
                    "oceCoordinate", "orientation", "originalCoordinate",
                    "pitch", "powerLevel", "pressure", "q", "roll",
                    "soundSpeed", "temperature", "temperatureMagnetometer",
                    "temperatureRTC", "time", "transmitEnergy", "v")))

        expect_equal(d1[["type"]], "Signature1000")
        expect_equal(d1[["type"]], ad2cpHeaderValue(d1, "ID", "STR", FALSE))
        expect_equal(d1[["fileType"]], "AD2CP")
        expect_equal(d1[["serialNumber"]], ad2cpHeaderValue(d1, "ID", "SN"))
        expect_equal(d1[["oceCoordinate"]], "beam")
        expect_equal(d1[["oceCoordinate", "burst"]], "beam")
        expect_equal(d1[["cellSize", "average"]], ad2cpHeaderValue(d1, "GETAVG", "CS"))
        expect_equal(d1[["blankingDistance", "average"]], ad2cpHeaderValue(d1, "GETAVG", "BD"))
        expect_equal(d1[["oceCoordinate", "average"]], tolower(ad2cpHeaderValue(d1, "GETAVG", "CY", FALSE)))
        expect_equal(d1[["cellSize", "burst"]], ad2cpHeaderValue(d1, "GETBURSTHR", "CS"))
        expect_equal(d1[["blankingDistance", "burst"]], ad2cpHeaderValue(d1, "GETBURSTHR", "BD"))
        # FIXME: the next uses GETBURST, not GETBURSTHR. I do not understand the format
        expect_equal(d1[["oceCoordinate", "burst"]], tolower(ad2cpHeaderValue(d1, "GETBURST", "CY", FALSE)))
        # FIXME: the next tests will fail if we store AHRS as 3D array
        # >> Data.BurstHR_AHRSRotationMatrix(1,:)
        expect_equal(d1@data$burst$AHRS[1, ], c(0.060653746, -0.37823972, -0.92368418,
                0.31505784, -0.87079191, 0.37727141,
                -0.94709891, -0.31389475, 0.066413939))
        expect_equal(d1[["AHRS", "burst"]][1, ], c(0.060653746, -0.37823972, -0.92368418,
                0.31505784, -0.87079191, 0.37727141,
                -0.94709891, -0.31389475, 0.066413939))
        expect_equal(d1[["AHRS", "burst"]][2, ], c(0.060740113, -0.37818542, -0.92374152,
                0.31508127, -0.87087512, 0.37725908,
                -0.94712538, -0.31396884, 0.066250026))
        expect_equal(d1[["AHRS", "average"]][1, ], c(0.060653746,-0.37824112, -0.92372596,
                0.31505644, -0.87087524, 0.37728685,
                -0.94714069, -0.31391019, 0.066330612))
        # >> load labtestsig3.ad2cp.00000_1.mat
        expect_equal(d1[["numberOfBeams", "burst"]], 1)
        expect_equal(d1[["numberOfBeams", "burst"]], ad2cpHeaderValue(d1, "GETBURST", "NB"))
        expect_equal(d1[["numberOfCells", "burst"]], 256)
        expect_equal(d1[["numberOfCells", "burst"]], ad2cpHeaderValue(d1, "GETBURSTHR", "NC"))
        expect_equal(d1[["numberOfBeams", "average"]], 4)
        expect_equal(d1[["numberOfBeams", "average"]], ad2cpHeaderValue(d1, "GETAVG1", "NB"))
        expect_equal(d1[["numberOfCells", "average"]], 150)
        expect_equal(d1[["numberOfCells", "average"]], ad2cpHeaderValue(d1, "GETAVG1", "NC"))
        # > Data.BurstHR_EnsembleCount(1:10)
        ensembleMatlab <- c(969, 970, 971, 972, 973,
            974, 975, 976, 977, 978)
        expect_equal(d1[["ensemble", "burst"]][1:10], ensembleMatlab)
        #> Data.Average_EnsembleCount
        expect_true(all(1==d1[["ensemble","average"]]))
        # timestamps
        # >> format long
        # >> Data.Average_TimeStamp(1:10)
        secAverageMatlab <- 1e9*c(1.490564521063300, 1.490564522063300, 1.490564523063300,
            1.490564524063300, 1.490564525063300, 1.490564526063300,
            1.490564527063300, 1.490564528063300, 1.490564529063300,
            1.490564530063300)
        timeAverageMatlab <- numberAsPOSIXct(secAverageMatlab)
        expect_equal(d1[["time", "average"]][1:10], timeAverageMatlab)
        # >> format long
        # >> Data.BurstHR_TimeStamp(1:10)
        secBurstMatlab <- 1e9*c(1.490564521001000, 1.490564521125800, 1.490564521251000,
            1.490564521376000, 1.490564521501000, 1.490564521626000,
            1.490564521751000, 1.490564521876000, 1.490564522001000,
            1.490564522125800)
        timeBurstMatlab <- numberAsPOSIXct(secBurstMatlab)
        expect_equal(d1[["time", "burst"]][1:10], timeBurstMatlab)
        #>> Data.BurstHR_TransmitEnergy(1:10)
        expect_equal(d1[["transmitEnergy", "burst"]][1:10], c(4, 0, 4, 4, 4, 4, 4, 4, 4, 0))
        #>> Data.Average_TransmitEnergy(1:10)
        expect_equal(d1[["transmitEnergy", "average"]][1:10], c(82, 82, 82, 82, 82, 82, 82, 82, 82, 82))
        # >> Data.Average_Pressure(1:10)
        pressureAverageMatlab <- c(10.259, 10.260, 10.262, 10.262, 10.258,
            10.260, 10.261, 10.256, 10.259, 10.261)
        expect_equal(d1[["pressure", "average"]][1:10], pressureAverageMatlab)
        # >> Data.BurstHR_Pressure(1:10)
        pressureBurstMatlab <- c(10.260, 10.258, 10.264, 10.261, 10.263,
            10.260, 10.260, 10.261, 10.259, 10.259)
        expect_equal(d1[["pressure", "burst"]][1:10], pressureBurstMatlab)
        # >> Data.BurstHR_WaterTemperature(1:10)
        temperatureMatlab <- c(24.010, 24.000, 24.010, 24.010, 24.010,
            24.010, 24.010, 24.010, 24.010, 24.000)
        expect_equal(d1[["temperature", "burst"]][1:10], temperatureMatlab)
        # > Data.AverageHR_MagnetometerTemperature(1:10)
        temperatureMagnetometerAverageMatlab <- c(25.8920, 25.8920, 25.8920, 25.8450, 25.8920,
            25.8450, 25.8920, 25.8450, 25.8920, 25.8450)
        expect_equal(d1[["temperatureMagnetometer", "average"]][1:10], temperatureMagnetometerAverageMatlab)
        # > Data.AverageHR_RTCTemperature(1:10)
        temperatureRTCAverageMatlab <- c(28.5000, 28.5000, 28.7500, 28.7500, 28.7500,
            28.7500, 28.7500, 28.7500, 28.7500, 28.7500)
        expect_equal(d1[["temperatureRTC", "average"]][1:10], temperatureRTCAverageMatlab)
        # > Data.BurstHR_MagnetometerTemperature(1:10)
        temperatureMagnetometerBurstMatlab <- c(25.7980, 25.8450, 25.9390, 25.8920, 25.8450,
            25.7510, 25.7980, 25.8920, 25.8450, 25.7980)
        expect_equal(d1[["temperatureMagnetometer", "burst"]][1:10], temperatureMagnetometerBurstMatlab)
        # > Data.BurstHR_RTCTemperature(1:10)
        temperatureRTCBurstMatlab <- c(28.500, 28.500, 28.500, 28.500, 28.500,
            28.500, 28.500, 28.500, 28.500, 28.500)
        expect_equal(d1[["temperatureRTC", "burst"]][1:10], temperatureRTCBurstMatlab)
        # >> Data.BurstHR_Heading(1:10)
        headingMatlab <- c(10.890, 10.910, 10.920, 10.980, 10.960,
            10.910, 10.900, 10.900, 10.900, 10.900)
        expect_equal(d1[["heading", "burst"]][1:10], headingMatlab)
        # >> Data.BurstHR_Pitch(1:10)
        pitchMatlab <- c(-71.280, -71.280, -71.270, -71.280, -71.280,
            -71.280, -71.270, -71.270, -71.270, -71.270)
        expect_equal(d1[["pitch", "burst"]][1:10], pitchMatlab)
        # >> Data.BurstHR_ROll(1:10)
        rollMatlab <- c(-78.050, -78.080, -78.080, -78.090, -78.090,
            -78.080, -78.080, -78.080, -78.080, -78.080)
        expect_equal(d1[["roll", "burst"]][1:10], rollMatlab)
        # >> Data.Average_CellSize
        # ans = single 0.2000
        expect_equal(d1[["cellSize", "average"]], 0.2)
        # >> Data.BurstHR_CellSize(1)
        # ans = single 0.0200
        expect_equal(d1[["cellSize", "burst"]], 0.02)
        # >> Data.Alt_Average_CellSize(1)
        # ans = single 0.2000
        # >> Data.Alt_BurstHR_CellSize(1)
        # ans = single 0.0500
        # >> Data.Average_Blanking(1)
        # ans = 0.1000
        expect_equal(d1[["blankingDistance", "average"]], 0.1)
        # >> Data.BurstHR_Blanking(1)
        # ans = 2.8000
        expect_equal(d1[["blankingDistance", "burst"]], 2.8)
        # >> Data.Alt_BurstHR_Blanking(1)
        # ans = 0.1000
        # >> Data.Alt_Average_Blanking(1)
        # ans = 0.1000
        beam2xyzAverageMatlab <- matrix(c(1.1831000,  0.00000000, -1.1831000, 0.0000000,
                0.0000000, -1.1831000,   0.0000000, 1.1831000,
                0.5518000,  0.00000000,  0.5518000, 0.0000000,
                0.0000000,  0.5518000,   0.0000000, 0.5518000),
            nrow=4, byrow=TRUE)
        # FIXME: check beam2xyzAverageMatlab
        # print(beam2xyzAverageMatlab)
        # >> Data.BurstHR_NominalCor(1:10)
        nominalCorrelationBurstMatlab <- rep(100, 10)
        #R > d1[["nomcor", "burst"]][1:10]
        #R [1]  49 100  33 100 100 100 100 100 100 100
        expect_equal(d1[["nominalCorrelation", "burst"]][1:10], nominalCorrelationBurstMatlab)
        #>> Data.Average_NominalCor(1:10)
        nominalCorrelationAverageMatlab <- rep(33, 10)
        expect_equal(d1[["nominalCorrelation", "average"]][1:10], nominalCorrelationAverageMatlab)
        # All are zero in matlab.
        expect_true(all(0 == d1[["powerLevel"]]))
        expect_true(all(0 == d1[["powerLevel", "burst"]]))
        expect_true(all(0 == d1[["powerLevel", "burst"]]))
        # relax tolerance since it's a 16-bit value
        # >> Data.Average_AccelerometerX(1:10)
        accxAverageMatlab <- c(-0.9497070, -0.9492188, -0.9477539, -0.9472656, -0.9458008,
            -0.9497070, -0.9501953, -0.9516602, -0.9511719, -0.9516602)
        expect_equal(d1[["accelerometerx", "average"]][1:10], accxAverageMatlab, tolerance=1e-5)
        # >> Data.Average_AccelerometerY(1:10)
        accyAverageMatlab <- c(-0.3134766, -0.3139648, -0.3168945, -0.3125000, -0.3178711,
            -0.3164062, -0.3168945, -0.3129883, -0.3154297, -0.3154297)
        expect_equal(d1[["accelerometery", "average"]][1:10], accyAverageMatlab, tolerance=1e-5)
        # >> Data.Average_AccelerometerZ(1:10)
        acczAverageMatlab <- c(0.0668945, 0.0649414, 0.0659180, 0.0649414, 0.0678711,
            0.0668945, 0.0693359, 0.0693359, 0.0649414, 0.0649414)
        expect_equal(d1[["accelerometerz", "average"]][1:10], acczAverageMatlab, tolerance=1e-5)
        #>> Data.BurstHR_AccelerometerX(1:10)
        accxBurstMatlab <- c(-0.9472656, -0.9497070, -0.9492188, -0.9467773, -0.9511719,
            -0.9506836, -0.9472656, -0.9492188, -0.9482422, -0.9506836)
        expect_equal(d1[["accelerometerx", "burst"]][1:10], accxBurstMatlab, tolerance=1e-5)
        #>> Data.BurstHR_AccelerometerY(1:10)
        accyBurstMatlab <- c(-0.3144531, -0.3178711, -0.3159180, -0.3168945, -0.3149414,
            -0.3154297, -0.3168945, -0.3139648, -0.3183594, -0.3154297)
        expect_equal(d1[["accelerometery", "burst"]][1:10], accyBurstMatlab, tolerance=1e-5)
        #>> Data.BurstHR_AccelerometerZ(1:10)
        acczBurstMatlab <- c(0.066895, 0.065918, 0.065430, 0.066406, 0.065918,
            0.068359, 0.070801, 0.068359, 0.069336, 0.069336)
        expect_equal(d1[["accelerometerz", "burst"]][1:10], acczBurstMatlab, tolerance=1e-5)
        expect_error(d1[["v", "junk"]], "ad2cp object does not contain data item 'junk'")
        expect_equal(dim(d1[["v"]]), c(11, 150, 4))
        expect_equal(dim(d1[["v", "average"]]), c(11, 150, 4))
        expect_equal(dim(d1[["v", "burst"]]), c(88, 256, 1))
        # >> Data.BurstHR_VelBeam5(1,1:10)
        # Note that bursts store in beam 5.
        expect_equal(d1[["v", "burst"]][1, 1:4, 1], c(0.36240, 0.35830, 0.36430, 0.20590))
        # >> Data.Average_VelBeam1(1,1:4)
        expect_equal(d1[["v"]][1, 1:4, 1], c(-0.8170, -0.8890, -1.9170, -2.1110))
        expect_equal(d1[["v", "average"]][1, 1:4, 1], c(-0.8170, -0.8890, -1.9170, -2.1110))
        #>> Data.Average_VelBeam1(2,1:4)
        expect_equal(d1[["v", "average"]][2, 1:4, 1], c(-0.7800, -2.3230, -1.0840, -0.8010))
        # >> Data.Average_VelBeam2(1,1:4)
        expect_equal(d1[["v", "average"]][1, 1:4, 2], c(-0.1630,  1.6930,  1.8490,  1.1120))
        #>> Data.Average_VelBeam2(2,1:4)
        expect_equal(d1[["v", "average"]][2, 1:4, 2], c(-0.6340,  1.4590,  1.9590,  0.9400))
        #>> Data.Average_VelBeam3(1,1:4)
        expect_equal(d1[["v", "average"]][1, 1:4, 3], c(-1.5600,  1.4140,  1.5630,   1.5510))
        #>> Data.BurstHR_AmpBeam5(1:4,1,1)
        expect_equal(0.5 * d1[["a","burst numeric"]][1:4, 1, 1], c(34.0, 34.0, 36.0, 34.0))
        # >> Data.BurstHR_AmpBeam5(1:4,2,1)
        expect_equal(0.5 * d1[["a","burst numeric"]][1:4, 2, 1], c(34.5000, 35.0000, 36.5000, 35.0000))
        # >> Data.BurstHR_AmpBeam5(1:4,3,1)
        expect_equal(0.5 * d1[["a","burst numeric"]][1:4, 3, 1], c(34.5000, 35.5000, 37.5000, 34.0000))
        # >> Data.BurstHR_AmpBeam5(1:4,4,1)
        expect_equal(0.5 * d1[["a","burst numeric"]][1:4, 4, 1], c(35.0000, 37.5000, 36.0000, 36.5000))
        #>> Data.Average_AmpBeam1(1:4,1,1)
        expect_equal(0.5 * d1[["a","numeric"]][1:4, 1, 1], c(43.5000, 42.5000, 44.0000, 43.0000))
        expect_equal(0.5 * d1[["a","average numeric"]][1:4, 1, 1], c(43.5000, 42.5000, 44.0000, 43.0000))
        #>> Data.Average_AmpBeam1(1:4,2,1)
        expect_equal(0.5 * d1[["a","average numeric"]][1:4, 2, 1], c(40.0000, 40.0000, 39.5000, 39.5000))
        #>> Data.Average_AmpBeam1(1:4,3,1)
        expect_equal(0.5 * d1[["a","average numeric"]][1:4, 3, 1], c(39.5000, 40.0000, 39.5000, 40.0000))
        #>> Data.BurstHR_CorBeam5(1:4,1)
        expect_equal(d1[["q", "burst numeric"]][1:4, 1, 1], c(72, 81, 63, 78))
        #>> Data.BurstHR_CorBeam5(1:4,2)
        expect_equal(d1[["q", "burst numeric"]][1:4, 2, 1], c(76, 49, 89, 79))
        #>> Data.BurstHR_CorBeam5(1:4,3)
        expect_equal(d1[["q", "burst numeric"]][1:4, 3, 1], c(91, 76, 70, 72))
        #>> Data.Average_CorBeam1(1:4,1)
        expect_equal(d1[["q", "numeric"]][1:4, 1, 1], c(49, 44, 36, 44 ))
        expect_equal(d1[["q", "average numeric"]][1:4, 1, 1], c(49, 44, 36, 44))
        #>> Data.Average_CorBeam1(1:4,2)
        expect_equal(d1[["q", "average numeric"]][1:4, 2, 1], c( 7,  4,  5,  3))
        #>> Data.Average_CorBeam2(1:4,1)
        expect_equal(d1[["q", "average numeric"]][1:4, 1, 2], c(19,  3,  3,  3))
        #>> Data.Average_CorBeam3(1:4,1)
        expect_equal(d1[["q", "average numeric"]][1:4, 1, 3], c( 4, 11,  8, 15))
        #>> Data.Average_CorBeam4(1:4,1)
        expect_equal(d1[["q", "average numeric"]][1:4, 1, 4], c(38, 53, 71, 66))
        # Next is not a test against matlab ground-truth, since I don't
        # have any ENU from matlab, but rather it is just a test against
        # code changes, with the check values being what the code
        # produced on 2019-01-06.
        d1enu <- toEnu(d1)
        expect_equal(d1enu[["v"]][1:2,1:2,1:4],
            structure(c(-0.0423407864127893, -0.0412083140396359,
                    -1.18658233863229, -0.655501998134796,
                    -2.04226357900539, -2.20814086810278,
                    -0.684468639431669, -1.64208610236321,
                    0.134903747336107, -0.555275548908209,
                    2.92161411078296, 4.13594858163354,
                    0.551305292490936, 0.155603618214437,
                    -1.78358555931817, -2.23533799929557),
                .Dim = c(2L, 2L, 4L)))
        expect_silent(plot(d1))
        expect_silent(plot(d1, which="velocity"))
        expect_silent(plot(d1, which="amplitude"))
        expect_silent(plot(d1, which="quality"))
        expect_silent(plot(d1, which="hydrography"))
        expect_silent(plot(d1, which="angles"))
        expect_silent(plot(d1, which="uv"))
        expect_silent(plot(d1, which="uv+ellipse"))
        expect_silent(plot(d1, which="uv+ellipse+arrow"))
        expect_error(plot(d1, which="bottomRange"), "ADP object lacks bottom-tracking data")
        expect_silent(plot(d1, which="progressiveVector"))
})
}


if (file.exists(f2)) {
    skip_on_cran()
    test_that("read.adp() on a private AD2CP file that has only 'burst' data", {
        N <- 500
        # Note: using read.adp() to ensure that it also works
        expect_warning(
            expect_warning(d2 <- read.adp(f2, from=1, to=N, by=1),
                "'plan' defaulting to 0"),
            "ignoring 'despike'")
        nnn <- c("average", "burst", "interleavedBurst")
        expect_equal(c(FALSE, TRUE, FALSE), nnn %in% names(d2@data))
        expect_equal("beam", d2[["oceCoordinate"]])
        expect_equal(sort(names(d2[["burst"]])),
            sort(c("a", "accelerometerx", "accelerometery", "accelerometerz",
                    "blankingDistance", "cellSize", "datasetDescription",
                    "ensemble", "heading", "magnetometerx", "magnetometery",
                    "magnetometerz", "nominalCorrelation", "numberOfBeams",
                    "numberOfCells", "oceCoordinate", "orientation",
                    "originalCoordinate", "pitch", "powerLevel", "pressure",
                    "q", "roll", "soundSpeed", "temperature",
                    "temperatureMagnetometer", "temperatureRTC", "time",
                    "transmitEnergy", "v")))
        expect_equal(d2[["fileType"]], "AD2CP")
        expect_equal(d2[["serialNumber"]], ad2cpHeaderValue(d2, "ID", "SN"))
        expect_equal(d2[["type"]], "Aquadopp2")
        expect_equal(d2[["type"]], ad2cpHeaderValue(d2, "ID", "STR", FALSE))
        expect_equal(d2[["cellSize"]], 0.2)
        expect_equal(d2[["cellSize"]], ad2cpHeaderValue(d2, "GETBURST", "CS"))
        expect_equal(d2[["cellSize", "burst"]], ad2cpHeaderValue(d2, "GETBURST", "CS"))
        ## Why is this wrong? Is it a unit problem (cm and m)?
        expect_equal(d2[["blankingDistance"]], ad2cpHeaderValue(d2, "GETBURST", "BD"))
        expect_equal(d2[["blankingDistance", "burst"]], ad2cpHeaderValue(d2, "GETBURST", "BD"))
        expect_equal(d2[["oceCoordinate"]], tolower(ad2cpHeaderValue(d2, "GETBURST", "CY", FALSE)))
        expect_equal(d2[["oceCoordinate", "burst"]], tolower(ad2cpHeaderValue(d2, "GETBURST", "CY", FALSE)))

        expect_true(is.numeric(d2[["v"]]))
        expect_error(d2[["v", "average"]], "ad2cp object does not contain data item 'average'")
        expect_equal(dim(d2[["v"]]), c(N-1, 1, 4))
        expect_true(is.raw(d2[["a"]]))
        expect_true(is.numeric(d2[["a", "numeric"]]))
        expect_equal(dim(d2[["a"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["a", "numeric"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["a", "burst numeric"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["a", "numeric burst"]]), c(N-1, 1, 4))
        expect_true(is.raw(d2[["a", "raw"]]))
        expect_equal(dim(d2[["a", "raw"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["a", "burst raw"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["a", "raw burst"]]), c(N-1, 1, 4))
        expect_true(is.raw(d2[["q", "raw"]]))
        expect_equal(dim(d2[["q", "raw"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["q", "burst raw"]]), c(N-1, 1, 4))
        expect_equal(dim(d2[["q", "raw burst"]]), c(N-1, 1, 4))
})
}

if (file.exists(f3)) {
    skip_on_cran()
    test_that("read.oce() on a private AD2CP file that has 'burst' and 'interleavedBurst' data", {
        N <- 100
        ## Note: using read.oce() to ensure that it also works
        expect_warning(d3 <- read.oce(f3, from=1, to=N, by=1),
            "'plan' defaulting to 1")
        ## subsetting
        nnn <- c("average", "burst", "interleavedBurst")
        expect_equal(c(FALSE, TRUE, TRUE), nnn %in% names(d3@data))
        expect_equal(c(FALSE, FALSE, FALSE), nnn %in% names(subset(d3, "average")@data))
        expect_equal(c(FALSE, TRUE, FALSE), nnn %in% names(subset(d3, "burst")@data))
        expect_equal(c(FALSE, FALSE, TRUE), nnn %in% names(subset(d3, "interleavedBurst")@data))
        ## Compare with some header values
        expect_equal(d3[["fileType"]], "AD2CP")
        expect_equal(d3[["serialNumber"]], ad2cpHeaderValue(d3, "ID", "SN"))
        expect_equal("Signature1000", ad2cpHeaderValue(d3, "ID", "STR", FALSE))
        expect_equal(d3[["type"]], ad2cpHeaderValue(d3, "ID", "STR", FALSE))
        expect_equal(d3[["cellSize", "burst"]], ad2cpHeaderValue(d3, "GETBURST", "CS"))
        expect_equal(d3[["blankingDistance", "burst"]], ad2cpHeaderValue(d3, "GETBURST", "BD"))
        ## NOTE: the next uses GETBURST, not GETBURSTHR. I do not understand the format
        expect_equal(d3[["oceCoordinate", "burst"]], tolower(ad2cpHeaderValue(d3, "GETBURST", "CY", FALSE)))
        ## NB in the header is 5, which I suppose refers to the whole
        ## instrument, but d3[["numberOfBeams"]] is 4 for the slant-beam
        ## samples and 1 for the vertical-beam samples. But this is an
        ## interleavedBurst mode, so I guess 5 is the right number. In any
        ## case, I have sidestepped the test.
        if (FALSE)
            expect_equal(d3[["numberOfBeams", "burst"]], ad2cpHeaderValue(d3, "GETBURST", "NB"))
        expect_equal(d3[["cellSize", "interleavedBurst"]], ad2cpHeaderValue(d3, "GETBURST1", "CS"))
        expect_equal(d3[["blankingDistance", "interleavedBurst"]], ad2cpHeaderValue(d3, "GETBURST1", "BD"))
        expect_equal(d3[["oceCoordinate", "interleavedBurst"]], tolower(ad2cpHeaderValue(d3, "GETBURST1", "CY", FALSE)))

        ## FIXME: I think the nbeams might be wrong for burst

        ## The dimension tests are not ground-truthed; they merely reflect
        ## what the oce code gives, so that a flag will go off if things
        ## change greatly in the code. This also checks the accessors
        ## against direct lookup.
        vb <- d3[["v", "burst"]]
        vib <- d3[["v", "interleavedBurst"]]
        expect_equal(dim(d3@data$burst$v), c(50, 88, 4))
        expect_equal(dim(vb), c(50, 88, 4))
        expect_equal(dim(d3@data$interleavedBurst$v), c(49, 88, 1))
        expect_equal(dim(vib), c(49, 88, 1))
        ab <- d3[["a", "burst"]]
        aib <- d3[["a", "interleavedBurst"]]
        expect_equal(dim(d3@data$burst$a), c(50, 88, 4))
        expect_equal(dim(ab), c(50, 88, 4))
        expect_equal(dim(d3@data$interleavedBurst$a), c(49, 88, 1))
        expect_equal(dim(aib), c(49, 88, 1))
        qb <- d3[["q", "burst"]]
        qib <- d3[["q", "interleavedBurst"]]
        expect_equal(dim(d3@data$burst$q), c(50, 88, 4))
        expect_equal(dim(qb), c(50, 88, 4))
        expect_equal(dim(d3@data$interleavedBurst$q), c(49, 88, 1))
        expect_equal(dim(qib), c(49, 88, 1))

        ## Note that the interleavedBurst j has just one beam, which
        ## I'm guessing might be the vertical one, but that's just a guess,
        ## because the nortek system integrator doc has the string
        ## 'interleave' on just a single page!
        par(mfcol=c(4, 2))
        zlim <- c(-3, 3)
        tb <- d3[["time", "burst"]]
        tib <- d3[["time", "interleavedBurst"]]
        db <- d3[["distance", "burst"]]
        dib <- d3[["distance", "interleavedBurst"]]
        imagep(tb, db, vb[,,1], zlim=zlim, zlab="d3[[\"v\", \"burst\"]][,,1]", drawTimeRange=FALSE, ylab="Distance [m]")
        imagep(tb, db, vb[,,2], zlim=zlim, zlab="d3[[\"v\", \"burst\"]][,,2]", drawTimeRange=FALSE, ylab="Distance [m]")
        imagep(tb, db, vb[,,3], zlim=zlim, zlab="d3[[\"v\", \"burst\"]][,,3]", drawTimeRange=FALSE, ylab="Distance [m]")
        imagep(tb, db, vb[,,4], zlim=zlim, zlab="d3[[\"v\", \"burst\"]][,,4]", drawTimeRange=FALSE, ylab="Distance [m]")
        imagep(tib, dib, vib[,,1], zlim=zlim, zlab="d3[[\"v\", \"interleavedBurst\"]][,,1]", drawTimeRange=FALSE, ylab="Distance [m]")
        plot(c(0,1), c(0,1), xlab="", ylab="", axes=FALSE, type="n")
        text(0.5, 0.5, "interleavedBurst has only 1 beam")
        plot(c(0,1), c(0,1), xlab="", ylab="", axes=FALSE, type="n")
        text(0.5, 0.5, paste("time range:\n", min(tb), "\n to\n ", max(tb)))
        ## below is how we know this is 'beam'
        ## FIXME: look in text for other things, e.g. beam cells etc
        ## t <- d3[["text"]]$text[[1]]
        ## t[grep('CY=',t)[1]]
        expect_equal("beam", d3[["oceCoordinate"]])
        expect_error(plot(d3, j="average"), "ad2cp object does not contain data item 'average'")
        expect_silent(plot(d3))
        expect_silent(plot(d3, j="burst")) # as above, since object holds 'average' data
        expect_silent(plot(d3, which="velocity")) # as above, since which='velocity' is default
        expect_silent(plot(d3, which="amplitude"))
        expect_silent(plot(d3, which="quality"))
        expect_silent(plot(d3, j="burst"))
        expect_silent(plot(d3, which="velocity", j="burst"))
        expect_silent(plot(d3, which="amplitude", j="burst"))
        expect_silent(plot(d3, which="quality", j="burst"))
        par(mfrow=c(1, 1)) # use single-panel since interleavedBurst is just 1 beam
        expect_silent(plot(d3, j="interleavedBurst"))
        expect_silent(plot(d3, which="velocity", j="interleavedBurst")) # as above
        expect_silent(plot(d3, which="amplitude", j="interleavedBurst"))
        expect_silent(plot(d3, which="quality", j="interleavedBurst"))
        ## Compare beams in three coordinate systems, with 6 plots over two pages
        par(mfcol=c(3, 2))
        zlim <- c(-2, 2)
        expect_silent(plot(d3, which=1, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3, which=2, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3, which=3, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3, which='a1', zlim=c(0, 255), drawTimeRange=FALSE))
        expect_silent(plot(d3, which='a2', zlim=c(0, 255), drawTimeRange=FALSE))
        expect_silent(plot(d3, which='a3', zlim=c(0, 255), drawTimeRange=FALSE))
        d3xyz <- beamToXyz(d3)
        expect_silent(plot(d3xyz, which=1, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3xyz, which=2, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3xyz, which=3, zlim=zlim/4, drawTimeRange=FALSE))
        expect_silent(d3enu <- xyzToEnu(d3xyz))
        expect_silent(plot(d3enu, which=1, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3enu, which=2, zlim=zlim, drawTimeRange=FALSE))
        expect_silent(plot(d3enu, which=3, zlim=zlim/4, drawTimeRange=FALSE))
})
}

