library(oce)
context("argo")

test_that("the data(argo) dataset", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              data(argo)
              expect_equal(argo[["id"]][1], "6900388")
              expect_equal(dim(argo[["pressure"]]), c(56,207))
              expect_equal(sort(names(argo[["data"]])),
                           c( "latitude", "longitude", "pressure",
                             "pressureAdjusted", "pressureAdjustedError",
                             "salinity", "salinityAdjusted",
                             "salinityAdjustedError", "temperature",
                             "temperatureAdjusted", "temperatureAdjustedError",
                             "time"))
          }
})


test_that("the data from which data(argo) was constructed", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              a <- read.oce("local_data/6900388_prof.nc")
              expect_equal(a[["id"]][1], "6900388")
              expect_equal(dim(a[["pressure"]]), c(56,207))
              expect_equal(sort(names(a[["data"]])),
                           c( "latitude", "longitude", "pressure",
                             "pressureAdjusted", "pressureAdjustedError",
                             "salinity", "salinityAdjusted",
                             "salinityAdjustedError", "temperature",
                             "temperatureAdjusted", "temperatureAdjustedError",
                             "time"))
          }
})

test_that("a bioargo dataset", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              a <- read.oce("local_data/BR5904179_001.nc")
              summary(a)
              expect_equal(a[["id"]][1], "5904179")
              expect_equal(dim(a[["pressure"]]), c(499,2))
              expect_equal(sort(names(a@data)),
                           c("bbp700", "bbp700Adjusted", "bbp700AdjustedError",
                             "betaBackscattering700", "bphaseOxygen",
                             "chlorophyllA", "chlorophyllAAdjusted",
                             "chlorophyllAAdjustedError",
                             "fluorescenceChlorophyllA", "latitude",
                             "longitude", "nitrate", "nitrateAdjusted",
                             "nitrateAdjustedError", "oxygen", "oxygenAdjusted",
                             "oxygenAdjustedError", "pressure",
                             "temperatureeratureOxygen", "time",
                             "UVIntensityDarkNitrate", "UVIntensityNitrate"))
          }
})

