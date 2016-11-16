library(oce)
context("argo")
test_that("argo with lower-case variable names", {
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

