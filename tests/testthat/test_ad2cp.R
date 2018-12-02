## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Nortek AD2CP")

test_that("read.ad2cp() on private file (compare with matlab)", {
            f <- "~/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
            if (file.exists(f)) {
              d <- read.ad2cp(f, 1, 100, 1)
              expect_equal(d[["serialNumber"]], 100159)
              ## >> load labtestsig3.ad2cp.00000_1.mat
              ## >> fieldnames(Data)

              context("numberOfBeams and numberOfCcells")
              expect_equal(d[["numberOfBeams", "burst"]], 1)
              expect_equal(d[["numberOfCells", "burst"]], 256)
              expect_equal(d[["numberOfBeams", "average"]], 4)
              expect_equal(d[["numberOfCells", "average"]], 150)


              context("ensemble")
              ##> Data.BurstHR_EnsembleCount(1:10)
              ensembleMatlab <- c(969, 970, 971, 972, 973,
                                  974, 975, 976, 977, 978)
              expect_equal(d[["ensemble", "burst"]][1:10], ensembleMatlab)
              ##> Data.Average_EnsembleCount
              expect_true(all(1==d[["ensemble","average"]]))


              context("timestamps")
              ## >> format long
              ## >> Data.Average_TimeStamp(1:10)
              secAverageMatlab <- 1e9*c(1.490564521063300, 1.490564522063300, 1.490564523063300,
                                        1.490564524063300, 1.490564525063300, 1.490564526063300,
                                        1.490564527063300, 1.490564528063300, 1.490564529063300,
                                        1.490564530063300)
              timeAverageMatlab <- numberAsPOSIXct(secAverageMatlab)
              expect_equal(d[["time", "average"]][1:10], timeAverageMatlab)
              ## >> format long
              ## >> Data.BurstHR_TimeStamp(1:10)
              secBurstMatlab <- 1e9*c(1.490564521001000, 1.490564521125800, 1.490564521251000,
                                      1.490564521376000, 1.490564521501000, 1.490564521626000,
                                      1.490564521751000, 1.490564521876000, 1.490564522001000,
                                      1.490564522125800)
              timeBurstMatlab <- numberAsPOSIXct(secBurstMatlab)
              expect_equal(d[["time", "burst"]][1:10], timeBurstMatlab)


              context("pressure")
              ## >> Data.Average_Pressure(1:10)
              pressureAverageMatlab <- c(10.259, 10.260, 10.262, 10.262, 10.258,
                                         10.260, 10.261, 10.256, 10.259, 10.261)
              expect_equal(d[["pressure", "average"]][1:10], pressureAverageMatlab)
              ## >> Data.BurstHR_Pressure(1:10)
              pressureBurstMatlab <- c(10.260, 10.258, 10.264, 10.261, 10.263,
                                       10.260, 10.260, 10.261, 10.259, 10.259)
              expect_equal(d[["pressure", "burst"]][1:10], pressureBurstMatlab)


              context("temperature")
              ## >> Data.BurstHR_WaterTemperature(1:10)
              temperatureMatlab <- c(24.010, 24.000, 24.010, 24.010, 24.010,
                                     24.010, 24.010, 24.010, 24.010, 24.000)
              expect_equal(d[["temperature", "burst"]][1:10], temperatureMatlab)
              ## > Data.AverageHR_MagnetometerTemperature(1:10)
              temperatureMagnetometerAverageMatlab <- c(25.8920, 25.8920, 25.8920, 25.8450, 25.8920,
                                                        25.8450, 25.8920, 25.8450, 25.8920, 25.8450)
              expect_equal(d[["temperatureMagnetometer", "average"]][1:10], temperatureMagnetometerAverageMatlab)
              ## > Data.AverageHR_RTCTemperature(1:10)
              temperatureRTCAverageMatlab <- c(28.5000, 28.5000, 28.7500, 28.7500, 28.7500,
                                               28.7500, 28.7500, 28.7500, 28.7500, 28.7500)
              expect_equal(d[["temperatureRTC", "average"]][1:10], temperatureRTCAverageMatlab)
              ## > Data.BurstHR_MagnetometerTemperature(1:10)
              temperatureMagnetometerBurstMatlab <- c(25.7980, 25.8450, 25.9390, 25.8920, 25.8450,
                                                      25.7510, 25.7980, 25.8920, 25.8450, 25.7980)
              expect_equal(d[["temperatureMagnetometer", "burst"]][1:10], temperatureMagnetometerBurstMatlab)
              ## > Data.BurstHR_RTCTemperature(1:10)
              temperatureRTCBurstMatlab <- c(28.500, 28.500, 28.500, 28.500, 28.500,
                                             28.500, 28.500, 28.500, 28.500, 28.500)
              expect_equal(d[["temperatureRTC", "burst"]][1:10], temperatureRTCBurstMatlab)


              context("heading, pitch and roll")
              ## >> Data.BurstHR_Heading(1:10)
              headingMatlab <- c(10.890, 10.910, 10.920, 10.980, 10.960,
                                 10.910, 10.900, 10.900, 10.900, 10.900)
              expect_equal(d[["heading", "burst"]][1:10], headingMatlab)
              ## >> Data.BurstHR_Pitch(1:10)
              pitchMatlab <- c(-71.280, -71.280, -71.270, -71.280, -71.280,
                               -71.280, -71.270, -71.270, -71.270, -71.270)
              expect_equal(d[["pitch", "burst"]][1:10], pitchMatlab)
              ## >> Data.BurstHR_ROll(1:10)
              rollMatlab <- c(-78.050, -78.080, -78.080, -78.090, -78.090,
                              -78.080, -78.080, -78.080, -78.080, -78.080)
              expect_equal(d[["roll", "burst"]][1:10], rollMatlab)


              context("cellSize")
              ## >> Data.Average_CellSize
              ## ans = single 0.2000
              expect_equal(d[["cellSize", "average"]], 0.2)
              ## >> Data.BurstHR_CellSize(1)
              ## ans = single 0.0200
              expect_equal(d[["cellSize", "burst"]], 0.02)


              context("blankingDistance")
              ## >> Data.Average_Blanking(1)
              ## ans = 0.1000
              expect_equal(d[["blankingDistance", "average"]], 0.1)
              ## >> Data.BurstHR_Blanking(1)
              ## ans = 2.8000
              expect_equal(d[["blankingDistance", "burst"]], 2.8)


              context("beam2xyz")
              ## > Config.avg_beam2xyz
              beam2xyzAverageMatlab <- matrix(c(1.1831000,  0.00000000, -1.1831000, 0.0000000,
                                                0.0000000, -1.1831000,   0.0000000, 1.1831000,
                                                0.5518000,  0.00000000,  0.5518000, 0.0000000,
                                                0.0000000,  0.5518000,   0.0000000, 0.5518000),
                                              nrow=4, byrow=TRUE)
              print("test_ad2cp.R below is the matrix from matlab\n")
              print(beam2xyzAverageMatlab)


              context("nominalCorrelation")
              ## >> Data.BurstHR_NominalCor(1:10)
              nominalCorrelationBurstMatlab <- rep(100, 10)
              ##R > d[["nomcor", "burst"]][1:10]
              ##R [1]  49 100  33 100 100 100 100 100 100 100
              expect_equal(d[["nominalCorrelation", "burst"]][1:10], nominalCorrelationBurstMatlab)
              ##>> Data.Average_NominalCor(1:10)
              nominalCorrelationAverageMatlab <- rep(33, 10)
              expect_equal(d[["nominalCorrelation", "average"]][1:10], nominalCorrelationAverageMatlab)


              context("acceleration")
              ## relax tolerance since it's a 16-bit value
              ## >> Data.Average_AccelerometerX(1:10)
              accxAverageMatlab <- c(-0.9497070, -0.9492188, -0.9477539, -0.9472656, -0.9458008,
                                     -0.9497070, -0.9501953, -0.9516602, -0.9511719, -0.9516602)
              expect_equal(d[["accelerometerx", "average"]][1:10], accxAverageMatlab, tolerance=1e-6)
              ## >> Data.Average_AccelerometerY(1:10)
              accyAverageMatlab <- c(-0.3134766, -0.3139648, -0.3168945, -0.3125000, -0.3178711,
                                     -0.3164062, -0.3168945, -0.3129883, -0.3154297, -0.3154297)
              expect_equal(d[["accelerometery", "average"]][1:10], accyAverageMatlab, tolerance=1e-6)
              ## >> Data.Average_AccelerometerZ(1:10)
              acczAverageMatlab <- c(0.0668945, 0.0649414, 0.0659180, 0.0649414, 0.0678711,
                                     0.0668945, 0.0693359, 0.0693359, 0.0649414, 0.0649414)
              expect_equal(d[["accelerometerz", "average"]][1:10], acczAverageMatlab, tolerance=1e-6)
              ##>> Data.BurstHR_AccelerometerX(1:10)
              accxBurstMatlab <- c(-0.9472656, -0.9497070, -0.9492188, -0.9467773, -0.9511719,
                                   -0.9506836, -0.9472656, -0.9492188, -0.9482422, -0.9506836)
              expect_equal(d[["accelerometerx", "burst"]][1:10], accxBurstMatlab, tolerance=1e-6)
              ##>> Data.BurstHR_AccelerometerY(1:10)
              accyBurstMatlab <- c(-0.3144531, -0.3178711, -0.3159180, -0.3168945, -0.3149414,
                                   -0.3154297, -0.3168945, -0.3139648, -0.3183594, -0.3154297)
              expect_equal(d[["accelerometery", "burst"]][1:10], accyBurstMatlab, tolerance=1e-6)
              ##>> Data.BurstHR_AccelerometerZ(1:10)
              acczBurstMatlab <- c(0.066895, 0.065918, 0.065430, 0.066406, 0.065918,
                                   0.068359, 0.070801, 0.068359, 0.069336, 0.069336)
              expect_equal(d[["accelerometerz", "burst"]][1:10], acczBurstMatlab, tolerance=1e-6)


              context("burst velocity")
              ## >> Data.BurstHR_VelBeam5(1,1:10)
              ## Note that bursts store in beam 5.
              expect_equal(d[["v", "burst"]][1, 1:4, 1], c(0.36240, 0.35830, 0.36430, 0.20590))


              context("average velocity")
              ## >> Data.Average_VelBeam1(1,1:4)
              expect_equal(d[["v"]][1, 1:4, 1], c(-0.8170, -0.8890, -1.9170, -2.1110))
              expect_equal(d[["v", "average"]][1, 1:4, 1], c(-0.8170, -0.8890, -1.9170, -2.1110))
              ##>> Data.Average_VelBeam1(2,1:4)
              expect_equal(d[["v", "average"]][2, 1:4, 1], c(-0.7800, -2.3230, -1.0840, -0.8010))
              ## >> Data.Average_VelBeam2(1,1:4)
              expect_equal(d[["v", "average"]][1, 1:4, 2], c(-0.1630,  1.6930,  1.8490,  1.1120))
              ##>> Data.Average_VelBeam2(2,1:4)
              expect_equal(d[["v", "average"]][2, 1:4, 2], c(-0.6340,  1.4590,  1.9590,  0.9400))
              ##>> Data.Average_VelBeam3(1,1:4)
              expect_equal(d[["v", "average"]][1, 1:4, 3], c(-1.5600,  1.4140,  1.5630,   1.5510))


              context("burst amplitude")
              ##>> Data.BurstHR_AmpBeam5(1:4,1,1)
              expect_equal(0.5 * d[['a','burst numeric']][1:4, 1, 1], c(34.0, 34.0, 36.0, 34.0))
              ## >> Data.BurstHR_AmpBeam5(1:4,2,1)
              expect_equal(0.5 * d[['a','burst numeric']][1:4, 2, 1], c(34.5000, 35.0000, 36.5000, 35.0000))
              ## >> Data.BurstHR_AmpBeam5(1:4,3,1)
              expect_equal(0.5 * d[['a','burst numeric']][1:4, 3, 1], c(34.5000, 35.5000, 37.5000, 34.0000))
              ## >> Data.BurstHR_AmpBeam5(1:4,4,1)
              expect_equal(0.5 * d[['a','burst numeric']][1:4, 4, 1], c(35.0000, 37.5000, 36.0000, 36.5000))


              context("average amplitude")
              ##>> Data.Average_AmpBeam1(1:4,1,1)
              expect_equal(0.5 * d[['a','numeric']][1:4, 1, 1], c(43.5000, 42.5000, 44.0000, 43.0000))
              expect_equal(0.5 * d[['a','average numeric']][1:4, 1, 1], c(43.5000, 42.5000, 44.0000, 43.0000))
              ##>> Data.Average_AmpBeam1(1:4,2,1)
              expect_equal(0.5 * d[['a','average numeric']][1:4, 2, 1], c(40.0000, 40.0000, 39.5000, 39.5000))
              ##>> Data.Average_AmpBeam1(1:4,3,1)
              expect_equal(0.5 * d[['a','average numeric']][1:4, 3, 1], c(39.5000, 40.0000, 39.5000, 40.0000))


              context("burst correlation")
              ##>> Data.BurstHR_CorBeam5(1:4,1)
              expect_equal(d[["q", "burst numeric"]][1:4, 1, 1], c(72, 81, 63, 78))
              ##>> Data.BurstHR_CorBeam5(1:4,2)
              expect_equal(d[["q", "burst numeric"]][1:4, 2, 1], c(76, 49, 89, 79))
              ##>> Data.BurstHR_CorBeam5(1:4,3)
              expect_equal(d[["q", "burst numeric"]][1:4, 3, 1], c(91, 76, 70, 72))


              context("average correlation")
              ##>> Data.Average_CorBeam1(1:4,1)
              expect_equal(d[["q", "numeric"]][1:4, 1, 1], c(49, 44, 36, 44 ))
              expect_equal(d[["q", "average numeric"]][1:4, 1, 1], c(49, 44, 36, 44))
              ##>> Data.Average_CorBeam1(1:4,2)
              expect_equal(d[["q", "average numeric"]][1:4, 2, 1], c( 7,  4,  5,  3))
              ##>> Data.Average_CorBeam2(1:4,1)
              expect_equal(d[["q", "average numeric"]][1:4, 1, 2], c(19,  3,  3,  3))
              ##>> Data.Average_CorBeam3(1:4,1)
              expect_equal(d[["q", "average numeric"]][1:4, 1, 3], c( 4, 11,  8, 15))
              ##>> Data.Average_CorBeam4(1:4,1)
              expect_equal(d[["q", "average numeric"]][1:4, 1, 4], c(38, 53, 71, 66))


              if (TRUE) {
                warning("skipping powerlevel (not coded yet)")
              } else {
                powerLevelMatlab <- rep(0, 10)
                expect_equal(d[["powerLevel"]][burstID][1:10], powerLevelMatlab)
              }

              if (TRUE) {
                warning("skipping transmitEnergy (not coded yet)")
              } else {
                ## >> Data.BurstHR_TransmitEnergy(1:10)
                transmitEnergyMatlab <- c(4, 0, 4, 4, 4,
                                          4, 4, 4, 4, 0)
                expect_equal(d[["transmitEnergy"]][burstID][1:10], transmitEnergyMatlab)
              }
            }
          })


