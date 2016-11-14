library(oce)
context("satellite")
test_that("amsr", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              amsr <- read.amsr("local_data/f34_20160808v7.2.gz")
              expect_equal("amsr", amsr[["spacecraft"]])
              expect_equal(c(1440, 720), dim(amsr[["SSTDay"]]))
              expect_equal(c("cloudDay", "cloudNight", "LFwindDay", "LFwindNight", "MFwindDay", "MFwindNight",
                             "rainDay", "rainNight", "SSTDay", "SSTNight", "timeDay", "timeNight", "vaporDay",
                             "vaporNight"), 
                           sort(names(amsr@data)))
              summary(amsr)
              plot(amsr)
          }
})

