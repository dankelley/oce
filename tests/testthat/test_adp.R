## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data(adp)

test_that("test all 'which' values listed in ?'plot,adp-method' on 2019 May 23", {
          ## Most of the numerical tests have character equivalents, but we
          ## test both. All this test really does is to ensure that the plots
          ## do not produce warnings, apart from known things, e.g. a warning
          ## if trying to plot bottom-tracking items, because data(adp) has
          ## no bottom-tracking data.
          for (which in c(1:4, 5:8, 9:12, 60, 70:73, 80:83, 13, 14, 15:18,
                          19:22, 23, 24:28, 29, 30, 40, 41:44, 50, 51:54, 55,
                          100)) {
              ## cat(file=stderr(), "plot(adp, which=", which, ")\n", sep="")
              if (which %in% c(40:44,50:54)) expect_error(plot(adp, which=which), "ADP object lacks bottom-tracking data")
              else if (which == 80)          expect_error(plot(adp, which=which), "ADP object lacks a 'vv' data item")
              else if (which == 81)          expect_error(plot(adp, which=which), "ADP object lacks a 'va' data item")
              else if (which == 82)          expect_error(plot(adp, which=which), "ADP object lacks a 'vq' data item")
              else if (which == 83)          expect_error(plot(adp, which=which), "ADP object lacks a 'vg' data item")
              else                           expect_silent( plot(adp, which=which))
          }
          for (which in c("u1", "u2", "u3", "u4", "a1", "a2", "a3", "a4", "q1",
                          "q2", "q3", "q4", "map", "g1", "g2", "g3", "g4", "vv",
                          "va", "vq", "vg", "vertical", "salinity",
                          "temperature", "pressure", "heading", "pitch", "roll",
                          "progressiveVector", "uv", "uv+ellipse",
                          "uv+ellipse+arrow", "bottomRange", "bottomRange1",
                          "bottomRange2", "bottomRange3", "bottomRange4",
                          "bottomVelocity1", "bottomVelocity2",
                          "bottomVelocity3", "bottomVelocity4",
                          "bottomVelocity", "heaving", "soundSpeed", "velocity",
                          "amplitude", "quality", "hydrography", "angles")) {
              ## cat(file=stderr(), "plot(adp, which=\"", which, "\")\n", sep="")
              if (length(grep("bottom", which))) expect_error(plot(adp, which=which), "ADP object lacks bottom-tracking data")
              else if (which == "vertical")      expect_error(plot(adp, which=which), "ADP object lacks a 'vv' data item")
              else if (which == "vv")            expect_error(plot(adp, which=which), "ADP object lacks a 'vv' data item")
              else if (which == "va")            expect_error(plot(adp, which=which), "ADP object lacks a 'va' data item")
              else if (which == "vq")            expect_error(plot(adp, which=which), "ADP object lacks a 'vq' data item")
              else if (which == "vg")            expect_error(plot(adp, which=which), "ADP object lacks a 'vg' data item")
              else                               expect_silent( plot(adp, which=which))
          }
})

test_that("some specialized plot types", {
          expect_silent(plot(adp, which=23, control=list('bin'=1)))
          expect_silent(plot(adp))
})

test_that("as.adp() inserts data properly", {
          data(adp)
          t <- adp[["time"]]
          d <- adp[["distance"]]
          v <- adp[["v"]]
          a <- as.adp(time=t, distance=d, v=v)
          expect_equal(a[["time"]], adp[["time"]])
          expect_equal(a[["distance"]], adp[["distance"]])
          expect_equal(a[["v"]], adp[["v"]])
})

test_that("adpEnsembleAverage() produces correctly-dimensioned results", {
          data(adp)
          n <- 5
          adpAvg <- adpEnsembleAverage(adp, n=n)
          expect_equal(length(adp[["time"]]), n*length(adpAvg[["time"]]))
          expect_equal(dim(adp[["v"]]), c(n, 1, 1) * dim(adpAvg[["v"]]))
          for (name in names(adp@data)) {
              if (is.vector(adp[[name]]) && "distance" != name) {
                  expect_equal(adpAvg[[name]][1], mean(adp[[name]][1:n]))
              }
          }
          expect_equal(adpAvg[["v"]][1,1,1], mean(adp[["v"]][1:n,1,1]))
          expect_equal(adpAvg[["v"]][1,2,1], mean(adp[["v"]][1:n,2,1]))
          expect_equal(adpAvg[["v"]][1,1,2], mean(adp[["v"]][1:n,1,2]))
          ## Test leftover bins: case 1, with a leftover
          adpAvg <- adpEnsembleAverage(adp, n=4, leftover=TRUE)
          nexpected <- 7               # have 6*4 full bins, plus 1 leftover bin
          expect_equal(length(adpAvg[["time"]]), nexpected)
          expect_equal(tail(adp[["time"]], 1), tail(adpAvg[["time"]], 1))
          expect_equal(dim(adpAvg[["v"]])[1], nexpected)
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["q"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["a"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["g"]]))
          ## Test leftover bins: case 2, with no leftover
          adpAvg <- adpEnsembleAverage(adp, n=4)
          nexpected <- 6               # because 6*4 < 25
          expect_equal(length(adpAvg[["time"]]), nexpected)
          expect_equal(dim(adpAvg[["v"]])[1], nexpected)
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["q"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["a"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["g"]]))
})

f <- "~/Dropbox/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000"
if (file.exists(f)) {
    test_that("details of a local RDI", {
              # args for read.adp.rdi() are file, from, to, by and tz
              d <- expect_silent(read.oce(f, 1, 3, 1, "UTC"))
              expect_equal(d[["time"]], as.POSIXct(c("2008-06-25 10:00:00",
                                                     "2008-06-25 10:00:10",
                                                     "2008-06-25 10:00:20"), tz="UTC"))

              expect_equal(d[["distance"]], c(2.23, 2.73, 3.23, 3.73, 4.23, 4.73, 5.23, 5.73, 6.23, 6.73, 7.23, 7.73, 8.23,
                                              8.73, 9.23, 9.73, 10.23, 10.73, 11.23, 11.73, 12.23, 12.73, 13.23, 13.73, 14.23,
                                              14.73, 15.23, 15.73, 16.23, 16.73, 17.23, 17.73, 18.23, 18.73, 19.23, 19.73,
                                              20.23, 20.73, 21.23, 21.73, 22.23, 22.73, 23.23, 23.73, 24.23, 24.73, 25.23,
                                              25.73, 26.23, 26.73, 27.23, 27.73, 28.23, 28.73, 29.23, 29.73, 30.23, 30.73,
                                              31.23, 31.73, 32.23, 32.73, 33.23, 33.73, 34.23, 34.73, 35.23, 35.73, 36.23,
                                              36.73, 37.23, 37.73, 38.23, 38.73, 39.23, 39.73, 40.23, 40.73, 41.23, 41.73,
                                              42.23, 42.73, 43.23, 43.73))

              v <- array(c(0.034, 0.073, 0.107, 0.049, -0.012, 0.11, 0.034, 0.024, 0.104,
                           -0.027, -0.015, 0.144, 0.038, 0.076, 0.227, -0.028, -0.02, 0.117,
                           0.048, -0.03, 0.187, -0.093, -0.033, 0.2, -0.071, 0.028, 0.166,
                           0.029, 0.115, 0.146, 0.081, 0.004, 0.219, 0.025, -0.074, 0.188,
                           -0.039, 0.11, 0.113, -0.07, 0.088, 0.249, 0.06, -0.055, 0.15,
                           -0.054, 0.019, 0.146, 0.046, -0.08, 0.098, -0.072, 0.013, 0.123,
                           -0.004, -0.06, 0.066, -0.058, 0.055, 0.152, -0.022, 0.037, 0.149,
                           0.007, -0.053, 0.139, 0.032, -0.007, 0.143, 0.113, -0.046, 0.063,
                           0.085, 0.001, 0.18, -0.046, -0.012, 0.097, -0.038, -0.034, 0.113,
                           -0.016, 0.061, 0.163, 0.017, 0.013, 0.197, 0, 0.092, 0.229,
                           0.015, 0.052, 0.137, -0.048, 0.185, 0.079, -0.029, 0.033, 0.099,
                           0.058, -0.039, 0.11, 0.016, 0.108, 0.036, -0.012, 0.084, 0.078,
                           0.014, 0.06, 0.044, 0.048, -0.023, 0.036, 0.058, 0.087, 0.078,
                           0.03, 0.039, 0.179, -0.074, 0.129, 0.065, 0.056, 0.13, 0.166,
                           -0.001, 0.173, 0.145, -0.135, 0.117, 0.057, 0.072, 0.025, 0.176,
                           -0.158, 0.096, 0.113, -0.022, 0.021, 0.04, 0.007, 0.015, -0.019,
                           -0.063, 0.167, 0.056, -0.104, 0.103, 0.217, 0.008, 0.137, 0.154,
                           -0.013, 0.156, 0.242, 0.033, 0.058, 0.083, -0.188, 0.179, 0.208,
                           -0.107, 0.087, 0.141, 0.037, 0.192, 0.152, -0.07, 0.185, 0.074,
                           -0.022, 0.345, 0.084, -0.078, 0.246, 0.193, -0.039, 0.22, 0.156,
                           0.019, 0.277, 0.107, 0.023, 0.296, 0.144, 0.121, 0.281, 0.131,
                           0.026, 0.305, 0.215, 0.048, 0.328, 0.225, -0.002, 0.269, 0.052,
                           0.079, 0.172, -0.025, 0.054, 0.148, 0.062, 0.1, 0.119, 0.134,
                           0.11, 0.277, -0.026, 0.209, 0.264, 0.022, 0.057, 0.168, -0.094,
                           0.051, 0.214, -0.06, 0.026, 0.251, 0.083, -0.038, 0.143, -0.162,
                           -0.025, 0.176, 0.023, 0.043, 0.233, 0.003, 0.028, 0.25, -0.119,
                           -0.058, 0.246, 0.035, 0.038, 0.197, -0.033, -0.053, 0.184, 0.014,
                           -0.023, 0.157, -0.038, 0.016, 0.177, -0.051, 0.045, 0.165,
                           -0.156, 0.035, 0.126, 0.042, 0.013, 0.045, 0.032, -0.009, 0.132,
                           0.047, -0.021, -0.01, 0.052, -0.094, 0.003, 0.073, 0, 0.07,
                           0.148, -0.019, 0.139, 0.116, -0.169, -0.052, 0.267, -0.126,
                           -0.011, 0.014, -0.136, -0.054, 0.025, -0.168, -0.071, 0.049,
                           -0.148, -0.104, 0.156, -0.11, 0.173, 0.105, -0.011, -0.034,
                           0.087, -0.112, -0.055, -0.069, -0.126, 0.047, 0.202, -0.03,
                           0.166, 0.208, -0.048, 0.026, -0.02, -0.152, -0.046, 0.173,
                           -0.063, -0.068, 0.154, -0.05, -0.058, 0.281, 0.004, -0.08, 0.163,
                           0.02, -0.008, 0.102, 0.012, -0.029, 0.216, -0.013, -0.066, 0.187,
                           0, 0.082, 0.206, -0.024, 0.061, 0.281, -0.097, 0.024, 0.261,
                           -0.028, 0.012, 0.227, -0.076, -0.036, 0.127, -0.2, -0.017, 0.174,
                           -0.017, -0.079, 0.291, -0.06, -0.178, 0.206, -0.131, -0.11, 0.21,
                           -0.05, -0.063, 0.141, -0.103, -0.22, 0.235, -0.071, -0.217,
                           0.262, -0.026, -0.037, 0.234, -0.063, -0.134, 0.243, 0.06,
                           -0.097, 0.199, -0.03, -0.136, 0.177, 0.029, -0.134, 0.103, 0.056,
                           0.019, 0.185, -0.084, -0.046, 0.096, 0.075, -0.131, 0.155,
                           -0.105, -0.169, 0.316, 0.065, -0.159, 0.106, -0.129, -0.049,
                           0.158, 0.046, 0.027, 0.272, 0.053, -0.053, 0.155, 0.086, -0.115,
                           0.154, 0.002, 0.016, 0.254, 0, 0.011, 0.257, 0.08, -0.049, 0.148,
                           0, -0.051, 0.206, -0.05, -0.08, 0.26, -0.007, 0.036, 0.161,
                           -0.047, 0.031, 0.246, 0.019, -0.065, 0.115, -0.097, -0.089,
                           0.151, -0.042, -0.016, 0.254, 0.017, -0.018, 0.228, 0.022,
                           -0.141, 0.291, -0.034, -0.003, 0.27, 0.108, 0.08, 0.235, 0.026,
                           -0.061, 0.253, 0.113, -0.128, 0.216, 0, 0.017, 0.172, -0.021,
                           -0.029, 0.327, -0.033, -0.048, 0.242, 0.104, 0.038, 0.152, 0.097,
                           0.029, 0.273, 0.096, -0.011, 0.279, 0.093, -0.116, 0.09, 0.073,
                           -0.014, 0.151, 0.159, 0.051, 0.164, 0.06, -0.027, 0.138, 0.056,
                           0.056, 0.189, -0.061, 0.034, 0.199, 0.057, 0.025, 0.084, -0.056,
                           -0.056, 0.115, -0.022, 0.085, 0.116, -0.045, 0.023, 0.066, 0.007,
                           -0.084, 0.015, 0.005, 0.07, 0.002, 0.081, 0.027, 0.179, 0.045,
                           -0.065, 0.048, 0.083, 0.005, 0.01, 0.026, 0.078, -0.053, 0.01,
                           -0.087, 0.035, -0.027, 0.026, 0.029, 0.082, 0.03, -0.026, 0.01,
                           -0.049, -0.022, 0.008, 0.05, -0.019, 0.105, -0.119, -0.055,
                           0.071, 0.005, 0.022, 0.049, -0.04, -0.021, 0.027, 0.095, -0.018,
                           -0.017, -0.005, -0.09, 0.155, -0.03, -0.045, 0.014, 0, -0.021,
                           0.061, -0.045, -0.169, 0.097, -0.161, -0.121, 0.033, -0.13,
                           -0.044, 0.078, -0.04, -0.133, 0.057, -0.003, -0.083, 0.035,
                           -0.115, -0.105, -0.02, -0.159, -0.167, -0.147, -0.108, -0.039,
                           -0.056, -0.089, -0.15, 0.063, -0.047, -0.113, -0.034, -0.093,
                           0.034, -0.018, -0.093, 0.035, 0.024, -0.034, -0.038, 0.078,
                           -0.119, -0.009, -0.036, -0.018, -0.057, 0.018, -0.006, 0.001,
                           0.061, -0.096, -0.022, -0.07, -0.102, 0.022, 0.054, -0.112, 0.04,
                           -0.104, -0.098, 0.052, 0.044, -0.04, 0.03, 0.113, -0.127, 0.01,
                           0.06, -0.065, -0.061, 0.025, -0.052, -0.017, 0.083, -0.051, 0,
                           0.061, 0.008, 0.041, -0.053, -0.003, -0.119, 0.029, -0.037,
                           -0.02, -0.102, 0.042, -0.084, 0.064, -0.076, -0.061, -0.019,
                           0.017, -0.062, -0.082, -0.036, -0.07, -0.016, 0.033, -0.101,
                           0.002, -0.041, 0.011, 0.026, -0.125, 0.1, -0.012, 0.004, 0,
                           -0.043, -0.013, 0.065, 0.007, 0.043, -0.009, -0.022, 0.004,
                           0.019, -0.064, 0.138, -0.057, 0.022, 0.003, 0.064, -0.054, 0.009,
                           0.077, -0.037, 0.015, 0.106, 0.024, -0.047, 0.069, -0.143,
                           -0.016, 0.162, -0.087, -0.085, 0.079, -0.134, -0.073, 0.075,
                           -0.077, -0.085, 0.043, -0.108, -0.08, 0.11, -0.095, -0.103,
                           0.022, -0.11, -0.002, -0.008, -0.204, -0.062, 0.03, -0.107,
                           -0.061, -0.034, 0.084, -0.159, 0.067, 0.031, -0.062, 0.057,
                           -0.08, -0.109, 0.095, -0.091, -0.105, 0.023, -0.092, -0.136,
                           0.18, -0.115, -0.101, 0.111, 0.038, -0.1, 0.081, 0.016, -0.096,
                           0.134, -0.057, -0.041, 0.068, -0.028, -0.111, 0.144, -0.1, 0.056,
                           0.15, -0.076, -0.084, 0.095, -0.015, -0.148, -0.004, -0.051,
                           0.013, 0.063, -0.018, -0.068, 0.087, -0.009, -0.027, -0.02,
                           -0.08, 0.016, -0.034, -0.085, 0.14, 0.086, 0.03, 0.014, -0.034,
                           -0.02, -0.064, -0.115, -0.088, 0.019, -0.035, -0.052, 0.098,
                           0.059, -0.04, 0.019, -0.062, 0.032, -0.082, -0.048, -0.016,
                           0.095, -0.005, -0.041, 0.001, -0.027, -0.117, 0.065, -0.097,
                           -0.024, -0.06, 0.013, 0.012, 0.059, -0.069, 0.04, 0.067, -0.06,
                           -0.083, 0.019, -0.168, -0.076, -0.005, 0.045, 0.012, 0.044,
                           -0.046, 0.034, 0.021, -0.06, 0.088, -0.011, -0.125, 0.001,
                           -0.006, -0.088, 0.004, 0.011, -0.081, 0.034, 0.003, 0.078, 0.071,
                           0.02, -0.021, -0.083, -0.039, -0.088, 0.042, -0.021, -0.105,
                           -0.005, -0.026, -0.067, -0.035, -0.043, -0.001, -0.027, -0.014,
                           -0.106, -0.133, 0.047, 0.018, -0.098, -0.054, 0.009, -0.1,
                           -0.058, -0.005, -0.074, -0.064, 0.037, 0.001, 0.016, -0.05,
                           -0.017, -0.058, 0.002, -0.18, -0.044, -0.051, -0.148, -0.019,
                           -0.22, -0.193, -0.055, -0.038, -0.091, -0.037, -0.005, -0.053,
                           -0.046, -0.154, -0.104, -0.012, -0.135, -0.14, -0.087, -0.171,
                           -0.144, -0.035, 0.047, -0.221, -0.003, -0.068, -0.09, -0.042,
                           -0.072, -0.067, 0.041, -0.085, -0.129, -0.008, -0.049, -0.088,
                           0.015, 0.013, -0.116, 0.054, 0.042, -0.203, 0.06, -0.083, -0.116,
                           -0.03, 0.004, -0.165, 0.09, -0.027, -0.051, 0.051, 0.031, -0.071,
                           -0.004, -0.007, -0.167, -0.113, 0.004, -0.102, -0.023, -0.058,
                           -0.165, -0.019, 0.051, -0.219, 0.06, 0.049, -0.121, 0.059,
                           -0.016, -0.199, 0.024, -0.056, -0.159, 0.065, 0.018, -0.047,
                           0.03, 0.029, -0.027, 0.075, -0.069, -0.1, 0.046, -0.048, -0.052,
                           0.151, 0.001, -0.055, 0.069, 0.029, -0.041, 0.172, 0.035, -0.099,
                           0.19, -0.043, -0.017, 0.101, 0.037, 0.031, 0.163, 0.017, -0.077,
                           0.033, -0.08, -0.088, 0.15, -0.012, -0.066, 0.145, -0.058,
                           -0.037, 0.058, 0.094, -0.164, 0.006, 0.091, -0.114, -0.015,
                           -0.026, -0.032, -0.048, -0.01, -0.162, -0.043, 0.13, -0.089,
                           -0.02, 0.063, -0.107, -0.018, 0.018, -0.136, 0.041, 0.1, -0.139,
                           -0.002, 0.02, -0.171, -0.042, 0.104), dim=c(3, 84, 4))
              expect_equal(d[["v"]], v)
})
}

test_that("three RDI reading methods (from, to, by not given)", {
          ## https://github.com/dankelley/oce/issues/1557
          adp1 <- read.oce(system.file("extdata", "adp_rdi.000", package="oce"))
          adp2 <- read.adp(system.file("extdata", "adp_rdi.000", package="oce"))
          adp3 <- read.adp.rdi(system.file("extdata", "adp_rdi.000", package="oce"))
          ## Do arrays match?
          for (item in c("v", "a", "g", "q")) {
              expect_equal(dim(adp1[[item]]), c(9, 84, 4))
              expect_equal(adp1[[item]], adp2[[item]])
              expect_equal(adp1[[item]], adp3[[item]])
          }
})

test_that("three RDI reading methods (from given)", {
          ## https://github.com/dankelley/oce/issues/1557
          adp1 <- read.oce(system.file("extdata", "adp_rdi.000", package="oce"), from=2)
          adp2 <- read.adp(system.file("extdata", "adp_rdi.000", package="oce"), from=2)
          adp3 <- read.adp.rdi(system.file("extdata", "adp_rdi.000", package="oce"), from=2)
          ## Do arrays match?
          for (item in c("v", "a", "g", "q")) {
              expect_equal(dim(adp1[[item]]), c(8, 84, 4))
              expect_equal(adp1[[item]], adp2[[item]])
              expect_equal(adp1[[item]], adp3[[item]])
          }
})

test_that("three RDI reading methods (from and by given)", {
          ## https://github.com/dankelley/oce/issues/1557
          adp1 <- read.oce(system.file("extdata", "adp_rdi.000", package="oce"), from=2, by=2)
          adp2 <- read.adp(system.file("extdata", "adp_rdi.000", package="oce"), from=2, by=2)
          adp3 <- read.adp.rdi(system.file("extdata", "adp_rdi.000", package="oce"), from=2, by=2)
          ## Do arrays match?
          for (item in c("v", "a", "g", "q")) {
              expect_equal(dim(adp1[[item]]), c(4, 84, 4))
              expect_equal(adp1[[item]], adp2[[item]])
              expect_equal(adp1[[item]], adp3[[item]])
          }
})

test_that("three RDI reading methods (from, by and to given)", {
          ## https://github.com/dankelley/oce/issues/1557
          adp1 <- read.oce(system.file("extdata", "adp_rdi.000", package="oce"), from=2, by=2, to=4)
          adp2 <- read.adp(system.file("extdata", "adp_rdi.000", package="oce"), from=2, by=2, to=4)
          adp3 <- read.adp.rdi(system.file("extdata", "adp_rdi.000", package="oce"), from=2, by=2, to=4)
          ## Do arrays match?
          for (item in c("v", "a", "g", "q")) {
              expect_equal(dim(adp1[[item]]), c(2, 84, 4))
              expect_equal(adp1[[item]], adp2[[item]])
              expect_equal(adp1[[item]], adp3[[item]])
          }
})

test_that("subset by time", {
          tmean <- mean(adp[["time"]])
          n <- sum(adp[["time"]] < tmean)
          earlyTime <- subset(adp, time < tmean)
          expect_equal(length(earlyTime[["orientation"]]), n)
          expect_equal(length(earlyTime[["ensembleNumber"]]), n)
          expect_equal(dim(earlyTime[["v"]])[1], n)
})

test_that("subset by ensembleNumber", {
          en <- adp[["ensembleNumber"]]
          n <- 10
          firstTen <- subset(adp, ensembleNumber < en[n+1])
          expect_equal(length(firstTen[["orientation"]]), n)
          expect_equal(length(firstTen[["ensembleNumber"]]), n)
          expect_equal(dim(firstTen[["v"]])[1], n)
})

