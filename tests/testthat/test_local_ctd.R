library(oce)
context("CTD")
test_that("ice-tethered profiler", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              itp <- read.ctd.itp("local_data/itp99grd0000.dat")
              expect_equal(itp[["latitude"]], 77.8840)
              expect_equal(itp[["longitude"]], 360 + (-145.0856))
          }
})

test_that("woce", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              woce <- read.ctd.woce("local_data/18HU2010014_00003_00001_ct1.csv")
              expect_equal(woce[["longitude"]], -52.5945)
              expect_equal(woce[["latitude"]], 47.5483)
              expect_equal(woce[["institute"]], "0513DFOBIOWGH")
              expect_equal(woce[["station"]], 3)
          }
})

