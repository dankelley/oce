## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Nortek AD2CP")

test_that("read.ad2cp() on private file (compare with matlab)", {
          f <- "~/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
          if (file.exists(f)) {
            d <- read.ad2cp(f, 1, 100, 1)
            ## summary(d) ## FIXME: THIS FAILS
            if (inherits(d, "oce")) {

              expect_equal(d[["serialNumber"]], 100159)
              ## >> load labtestsig3.ad2cp.00000_1.mat
              ## >> fieldnames(Data)
              ## >> Data.BurstHR_Pvaluesure(1:10)
              pressureMatlab <- c(10.260, 10.258, 10.264, 10.261, 10.263,
                                  10.260, 10.260, 10.261, 10.259, 10.259)
              expect_equal(d[["pressure"]][d[["id"]]==21][1:10], pressureMatlab)

              ## >> Data.BurstHR_WaterTemperature(1:10)
              temperatureMatlab <- c(24.010, 24.000, 24.010, 24.010, 24.010,
                                     24.010, 24.010, 24.010, 24.010, 24.000)
              expect_equal(d[["temperature"]][d[["id"]]==21][1:10], temperatureMatlab)

              ## >> Data.BurstHR_Heading(1:10)
              headingMatlab <- c(10.890, 10.910, 10.920, 10.980, 10.960,
                                 10.910, 10.900, 10.900, 10.900, 10.900)
              expect_equal(d[["heading"]][d[["id"]]==21][1:10], headingMatlab)

              ## >> Data.BurstHR_Pitch(1:10)
              pitchMatlab <- c(-71.280, -71.280, -71.270, -71.280, -71.280,
                               -71.280, -71.270, -71.270, -71.270, -71.270)
              expect_equal(d[["pitch"]][d[["id"]]==21][1:10], pitchMatlab)

              ## >> Data.BurstHR_ROll(1:10)
              rollMatlab <- c(-78.050, -78.080, -78.080, -78.090, -78.090,
                              -78.080, -78.080, -78.080, -78.080, -78.080)
              expect_equal(d[["roll"]][d[["id"]]==21][1:10], rollMatlab)

              cellSizeMatlab <- rep(0.02, 10)
              expect_equal(d[["cellSize"]][d[["id"]]==21][1:10], cellSizeMatlab)

              ## NOTE dividing by 10 to check against matlab
              blankingMatlab <- rep(2.8000, 10)
              expect_equal(d[["blanking"]][d[["id"]]==21][1:10], blankingMatlab)

              ## >> Data.Alt_BurstHR_NominalCor(1:10)
              nominalCorrelationMatlab <- c(100, 100, 100, 100, 100,
                                            100, 100, 100, 100, 100)
              expect_equal(d[["nomcor"]][d[["id"]]==21][1:10], nominalCorrelationMatlab)
              ##>> Data.Alt_Average_NominalCor(1:6)
              avgNominalCorrelationMatlab <- c(33, 33, 33, 33, 33, 33)
              expect_equal(d[["nomcor"]][d[["id"]]==22][1:6], avgNominalCorrelationMatlab)

              ##>> Data.BurstHR_AccelerometerZ(1:10)
              acczMatlab <- c(0.066895, 0.065918, 0.065430, 0.066406, 0.065918,
                              0.068359, 0.070801, 0.068359, 0.069336, 0.069336)
              ## relax tolerance since it's a 16-bit value
              expect_equal(d[["accz"]][d[["id"]]==21][1:10], acczMatlab, tolerance=1e-6)

              powerLevelMatlab <- rep(0, 10)
              expect_equal(d[["powerLevel"]][d[["id"]]==21][1:10], powerLevelMatlab)

              ## >> Data.BurstHR_TransmitEnergy(1:10)
              transmitEnergyMatlab <- c(4, 0, 4, 4, 4,
                                        4, 4, 4, 4, 0)
              expect_equal(d[["transmitEnergy"]][d[["id"]]==21][1:10], transmitEnergyMatlab)

              ## > Data.BurstHR_RTCTemperature(1:10)
              temperatureRTCMatlab <- c(28.500, 28.500, 28.500, 28.500, 28.500,
                                        28.500, 28.500, 28.500, 28.500, 28.500)
              expect_equal(d[["temperatureRTC"]][d[["id"]]==21][1:10], temperatureRTCMatlab)

              ##> Data.BurstHR_EnsembleCount(1:10)
              ensembleMatlab <- c(969, 970, 971, 972, 973,
                                  974, 975, 976, 977, 978)
              expect_equal(d[["ensemble"]][d[["id"]]==21][1:10], ensembleMatlab)

              ## >> output_precision(25)
              ## >> Data.BurstHR_TimeStamp(1:10)
              ts <- c(1.490564521001000165939331e+09, 1.490564521125800132751465e+09, 1.490564521251000165939331e+09,
                      1.490564521376000165939331e+09, 1.490564521501000165939331e+09, 1.490564521626000165939331e+09,
                      1.490564521751000165939331e+09, 1.490564521876000165939331e+09, 1.490564522001000165939331e+09,
                      1.490564522125800132751465e+09)
              timeMatlab <- numberAsPOSIXct(ts)
              expect_equal(d[["time"]][d[["id"]]==21][1:10], timeMatlab)

              ## >> Data.BurstHR_MagnetometerTemperature(1:10)
              temperatureMagnetometerMatlab <- c(2.579800034e+01, 2.584499931e+01, 2.593899918e+01, 2.589200020e+01,
                                                 2.584499931e+01, 2.575099945e+01, 2.579800034e+01, 2.589200020e+01,
                                                 2.584499931e+01, 2.579800034e+01)
              expect_equal(d[["temperatureMagnetometer"]][d[["id"]]==21][1:10],
                           temperatureMagnetometerMatlab, tolerance=1e-5)

              context("nbeams and ncells")
              expect_equal(d[["nbeams"]][d[["id"]]==21][1], 1)
              expect_equal(d[["ncells"]][d[["id"]]==21][1], 256)
              expect_equal(d[["nbeams"]][d[["id"]]==22][1], 4)
              expect_equal(d[["ncells"]][d[["id"]]==22][1], 150)

              ## Ensemble 2 is "burst" mode (beam5)
              ## >> Data.BurstHR_VelBeam5(1,1:10)
              v <- c(0.36240, 0.35830, 0.36430, 0.20590, 0.35690, 0.35650, 0.35730, 0.36090, 0.36390, 0.36600)
              expect_equal(d[["v"]][[2]][1:10], v, tolerance=1e-5)

              ## Ensemble 3 is in "average" mode.
              ## >> Data.Average_VelBeam1(1,1:10)
              v <- c(-0.81700,-0.88900,-1.91700,-2.11100,-1.00000,-2.08900,-1.54000,-0.85800,-1.93400,-1.56100)
              expect_equal(d[["v"]][[3]][1:10,1], v)

              ## >> Data.Average_VelBeam2(1,1:10)
              v <- c(-0.16300,1.69300,1.84900,1.11200,1.57300,-1.50400,1.60000,-2.52800,1.72100,1.68400)
              expect_equal(d[["v"]][[3]][1:10,2], v)

              ## >> Data.Average_VelBeam3(1,1:10)
              v <- c(-1.56000,1.41400,1.56300,1.55100,-0.32300,-1.27200,-2.11300,-1.28600,-2.36900,-2.38800)
              expect_equal(d[["v"]][[3]][1:10,3], v)

              ## >> Data.Average_VelBeam4(1,1:10)
              v <- c(-0.079000,1.522000,1.587000,1.702000,1.674000,1.230000,2.855000,2.999000,2.913000,1.486000)
              expect_equal(d[["v"]][[3]][1:10,4], v)

              if (FALSE) { # plot some image
                par(mfrow=c(3,3))
                for (i in 1:9)
                  imagep(d[["v"]][[i+1]])
                par(mfrow=c(1,1))
              }

              ## Ensemble 2 is burst mode.
              ## The bursts are just beam 5.
              ## >> Data.BurstHR_VelBeam5(1,1:10)
              v <- c(0.36240, 0.35830, 0.36430, 0.20590, 0.35690, 0.35650, 0.35730, 0.36090, 0.36390, 0.36600)
              expect_equal(d[["v"]][[2]][1:10], v, tolerance=1e-5)

              ## Ensemble 3 is average mode.
              ## >> Data.Average_VelBeam1(1,1:10)
              v <- c(-0.81700,-0.88900,-1.91700,-2.11100,-1.00000,-2.08900,-1.54000,-0.85800,-1.93400,-1.56100)
              expect_equal(d[["v"]][[3]][1:10,1], v, tolerance=1e-5)

              ## >> Data.Average_VelBeam2(1,1:10)
              v <- c(-0.16300,1.69300,1.84900,1.11200,1.57300,-1.50400,1.60000,-2.52800,1.72100,1.68400)
              expect_equal(d[["v"]][[3]][1:10,2], v, tolerance=1e-5)

              ## >> Data.Average_VelBeam3(1,1:10)
              v <- c(-1.56000,1.41400,1.56300,1.55100,-0.32300,-1.27200,-2.11300,-1.28600,-2.36900,-2.38800)
              expect_equal(d[["v"]][[3]][1:10,3], v, tolerance=1e-5)

              ## >> Data.Average_VelBeam4(1,1:10)
              v <- c(-0.079000,1.522000,1.587000,1.702000,1.674000,1.230000,2.855000,2.999000,2.913000,1.486000)
              expect_equal(d[["v"]][[3]][1:10,4], v, tolerance=1e-5)
            }
          }
})


