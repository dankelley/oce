library(oce)
test_that("amsr", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              amsr <- read.amsr("local_data/f34_20160808v7.2.gz")
              expect_equal("amsr", amsr[["spacecraft"]])
              expect_equal(c(1440, 720), dim(amsr[["SSTDay"]]))
              expect_equal(tolower(c("cloudDay", "cloudNight", "LFwindDay", "LFwindNight", "MFwindDay", "MFwindNight",
                             "rainDay", "rainNight", "SSTDay", "SSTNight", "timeDay", "timeNight", "vaporDay",
                             "vaporNight")),
                           sort(tolower(names(amsr@data))))
              ##> summary(amsr)
              expect_warning(plot(amsr), "auto-decimating first index of large image by 3")
          }
})

