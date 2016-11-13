library(oce)
context("CTD")
test_that("ice-tethered profiler", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              itp <- read.ctd.itp("local_data/itp99grd0000.dat")
              summary(itp)
              plot(itp)
              expect_equal(itp[["latitude"]], 77.8840)
              expect_equal(itp[["longitude"]], 360 + (-145.0856))
          }
})

