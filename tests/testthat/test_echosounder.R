library(oce)
context("echosounder")
f <- "~/Dropbox/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4"
if (file.exists(f)) {
    test_that("private biosonics file", {
              echosounder <- read.oce(f)
              echosounder <- subset(echosounder, depth < 40)
              echosounder <- decimate(echosounder, c(2, 40))
              expect_equal("single-beam", echosounder[["beamType"]])
              expect_equal(dim(echosounder[["a"]]), c(389, 54))
              expect_equal(echosounder[["a"]][10,10:15],
                           c(1101.6125, 818.9500, 892.3500, 1393.0625, 2320.9500, 5840.2750))
              expect_equal(echosounder[["a"]][10:15,10],
                           c(1101.6125, 1164.5500, 875.3750, 842.5750, 959.6250, 1076.6500))
              expect_equal(head(echosounder[["time"]]),
                           as.POSIXct(c("2008-07-01 16:39:41.019", "2008-07-01 16:39:41.509",
                                        "2008-07-01 16:39:42.000", "2008-07-01 16:39:42.485",
                                        "2008-07-01 16:39:42.974", "2008-07-01 16:39:43.464"),
                                      tz="UTC"))
              expect_equal(head(echosounder[["latitude"]]),
                           c(47.87948333, 47.87948333, 47.87948333, 47.87948825, 47.87949642,
                             47.87950000))
              expect_equal(head(echosounder[["longitude"]]),
                           c(-69.72364436, -69.72366061, -69.72367686, -69.72368808,
                             -69.72369625, -69.72370900))
              expect_silent(plot(echosounder))
})
}

