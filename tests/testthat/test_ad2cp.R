## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Nortek AD2CP")

test_that("read.ad2cp() on private file (compare with matlab)", {
          f <- "~/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
          if (file.exists(f)) {
            d <- read.ad2cp(f, 1, 100, 1)
            if (inherits(d, "oce")) {
              expect_equal(d[["serialNumber"]], 100159)
              burstID <- d[["id"]] == 21
              averageID <- d[["id"]] == 22
              ## >> load labtestsig3.ad2cp.00000_1.mat
              ## >> fieldnames(Data)
              ## >> Data.BurstHR_Pressure(1:10)
              pressureMatlab <- c(10.260, 10.258, 10.264, 10.261, 10.263,
                                  10.260, 10.260, 10.261, 10.259, 10.259)
              expect_equal(d[["pressure", "burst"]][1:10], pressureMatlab)

              ## >> Data.BurstHR_WaterTemperature(1:10)
              temperatureMatlab <- c(24.010, 24.000, 24.010, 24.010, 24.010,
                                     24.010, 24.010, 24.010, 24.010, 24.000)
              expect_equal(d[["temperature", "burst"]][1:10], temperatureMatlab)

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

              ## >> Data.Average_CellSize
              ## ans = single 0.2000
              expect_equal(d[["cellSize", "average"]], 0.2)
              ## >> Data.BurstHR_CellSize(1)
              ## ans = single 0.0200
              expect_equal(d[["cellSize", "burst"]], 0.02)

              ## >> Data.Average_Blanking(1)
              ## ans = 0.1000
              expect_equal(d[["blankingDistance", "average"]], 0.1)
              ## >> Data.BurstHR_Blanking(1)
              ## ans = 2.8000
              expect_equal(d[["blankingDistance", "burst"]], 2.8)

              ## >> Data.BurstHR_NominalCor(1:10)
              nominalCorrelationBurstMatlab <- rep(100, 10)
              ##R > d[["nomcor", "burst"]][1:10]
              ##R [1]  49 100  33 100 100 100 100 100 100 100
              expect_equal(d[["nominalCorrelation", "burst"]][1:10], nominalCorrelationBurstMatlab)
              ##>> Data.Average_NominalCor(1:10)
              nominalCorrelationAverageMatlab <- rep(33, 10)
              expect_equal(d[["nominalCorrelation", "average"]][1:10], nominalCorrelationAverageMatlab)

              ##>> Data.BurstHR_AccelerometerZ(1:10)
              acczMatlab <- c(0.066895, 0.065918, 0.065430, 0.066406, 0.065918,
                              0.068359, 0.070801, 0.068359, 0.069336, 0.069336)
              ## relax tolerance since it's a 16-bit value
              expect_equal(d[["accelerometerz", "burst"]][1:10], acczMatlab, tolerance=1e-6)

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

              ##> Data.BurstHR_EnsembleCount(1:10)
              ensembleMatlab <- c(969, 970, 971, 972, 973,
                                  974, 975, 976, 977, 978)
              expect_equal(d[["ensemble", "burst"]][1:10], ensembleMatlab)
              ##> Data.Average_EnsembleCount
              expect_true(all(1==d[["ensemble","average"]]))

              context("timestamps")
              ## >> format long
              ## >> Data.Average_TimeStamp(1:10)
              secAverageMatlab <- 1e9*c(1.490564521063300, 1.490564522063300, 1.490564523063300, 1.490564524063300,
                                        1.490564525063300, 1.490564526063300, 1.490564527063300, 1.490564528063300,
                                        1.490564529063300, 1.490564530063300)
              timeAverageMatlab <- numberAsPOSIXct(secAverageMatlab)
              expect_equal(d[["time", "average"]][1:10], timeAverageMatlab)

              ## >> format long
              ## >> Data.BurstHR_TimeStamp(1:10)
              secBurstMatlab <- 1e9*c(1.490564521001000, 1.490564521125800, 1.490564521251000, 1.490564521376000,
                                      1.490564521501000, 1.490564521626000, 1.490564521751000, 1.490564521876000,
                                      1.490564522001000, 1.490564522125800)
              timeBurstMatlab <- numberAsPOSIXct(secBurstMatlab)
              expect_equal(d[["time", "burst"]][1:10], timeBurstMatlab)

              context("nbeams and ncells")
              expect_equal(d[["numberOfBeams", "burst"]], 1)
              expect_equal(d[["numberOfCells", "burst"]], 256)
              expect_equal(d[["numberOfBeams", "average"]], 4)
              expect_equal(d[["numberOfCells", "average"]], 150)

              if (TRUE) {
                warning("several velo tests -- do we already have them, though?")
              } else {
                ## Ensemble 2 is "burst" mode (beam5)
                ## >> Data.BurstHR_VelBeam5(1,1:10)
                v <- oceGetData(d, "v") # NOT the same as d[["v"]]
                vv <- c(0.36240, 0.35830, 0.36430, 0.20590, 0.35690, 0.35650, 0.35730, 0.36090, 0.36390, 0.36600)
                expect_equal(d@data$v[[2]][1:10], vv, tolerance=1e-5)
                expect_equal(v[[2]][1:10], vv, tolerance=1e-5)

                ## Ensemble 3 is in "average" mode.
                ## >> Data.Average_VelBeam1(1,1:10)
                vv <- c(-0.81700,-0.88900,-1.91700,-2.11100,-1.00000,-2.08900,-1.54000,-0.85800,-1.93400,-1.56100)
                expect_equal(v[[3]][1:10,1], vv)

                ## >> Data.Average_VelBeam2(1,1:10)
                vv <- c(-0.16300,1.69300,1.84900,1.11200,1.57300,-1.50400,1.60000,-2.52800,1.72100,1.68400)
                expect_equal(v[[3]][1:10,2], vv)

                ## >> Data.Average_VelBeam3(1,1:10)
                vv <- c(-1.56000,1.41400,1.56300,1.55100,-0.32300,-1.27200,-2.11300,-1.28600,-2.36900,-2.38800)
                expect_equal(v[[3]][1:10,3], vv)

                ## >> Data.Average_VelBeam4(1,1:10)
                vv <- c(-0.079000,1.522000,1.587000,1.702000,1.674000,1.230000,2.855000,2.999000,2.913000,1.486000)
                expect_equal(v[[3]][1:10,4], vv)


                ## Ensemble 2 is burst mode.
                ## The bursts are just beam 5.
                ## >> Data.BurstHR_VelBeam5(1,1:10)
                vv <- c(0.36240, 0.35830, 0.36430, 0.20590, 0.35690, 0.35650, 0.35730, 0.36090, 0.36390, 0.36600)
                expect_equal(d@data$v[[2]][1:10], vv, tolerance=1e-5)

                ## Ensemble 3 is average mode.
                ## >> Data.Average_VelBeam1(1,1:10)
                vv <- c(-0.81700,-0.88900,-1.91700,-2.11100,-1.00000,-2.08900,-1.54000,-0.85800,-1.93400,-1.56100)
                expect_equal(d@data$v[[3]][1:10,1], vv, tolerance=1e-5)

                ## >> Data.Average_VelBeam2(1,1:10)
                vv <- c(-0.16300,1.69300,1.84900,1.11200,1.57300,-1.50400,1.60000,-2.52800,1.72100,1.68400)
                expect_equal(d@data$v[[3]][1:10,2], vv, tolerance=1e-5)

                ## >> Data.Average_VelBeam3(1,1:10)
                vv <- c(-1.56000,1.41400,1.56300,1.55100,-0.32300,-1.27200,-2.11300,-1.28600,-2.36900,-2.38800)
                expect_equal(d@data$v[[3]][1:10,3], vv, tolerance=1e-5)

                ## >> Data.Average_VelBeam4(1,1:10)
                vv <- c(-0.079000,1.522000,1.587000,1.702000,1.674000,1.230000,2.855000,2.999000,2.913000,1.486000)
                expect_equal(d@data$v[[3]][1:10,4], vv, tolerance=1e-5)
              }
            }
          }
})


