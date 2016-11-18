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

## I dump files here whenever I download new data. These files
## go back about 3 years, I think.
test_that("various ctd files", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              files <- c("77DN20020420_hy1.csv",
                         "p10_00026_00001_ct1.csv",
                         "sr01_l_00001_00003_ct1.csv",
                         "p02_2004a_00175_00002_ct1.csv",
                         "i06sb_00062_00001_ct1.csv",
                         "a23_00043_00001_ct1.csv",
                         "a22_00025_00001_ct1.csv",
                         "a03_3_00001_ct1.csv",
                         "a22_2003a_00001_00001_ct1.csv",
                         "18HU2010014_00003_00001_ct1.csv",
                         "18HU20130507_00235_00001_ct1.csv")
              for (file in files) {
                  d <- read.oce(paste("local_data", file, sep="/"))
                  ## summarizing and plotting can depend on the data, so try both
                  summary(d)
                  plot(d)
              }
          }
})

