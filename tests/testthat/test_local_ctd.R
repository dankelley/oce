library(oce)
context("local CTD")
test_that("ice-tethered profiler", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              itp <- read.ctd.itp("local_data/itp99grd0000.dat")
              expect_equal(itp[["latitude"]], 77.8840)
              expect_equal(itp[["longitude"]], 360 + (-145.0856))
          }
})

test_that("woce 1", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              woce <- read.ctd.woce("local_data/18HU2010014_00003_00001_ct1.csv")
              expect_equal(woce[["longitude"]], -52.5945)
              expect_equal(woce[["latitude"]], 47.5483)
              expect_equal(woce[["station"]], 3)
          }
})

test_that("woce 2", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              woce <- read.ctd.woce("local_data/example_ct1.csv")
              expect_equal(woce[["latitude"]], -17.5102)
              expect_equal(woce[["longitude"]], -150.4812)
              expect_equal(woce[["station"]], 221)
              expect_equal(woce[["waterDepth"]], 3596)
              expect_equal(woce[["pressureUnit"]], list(unit=expression(dbar), scale=""))
              expect_equal(woce[["temperatureUnit"]], list(unit=expression(degree*C), scale="ITS-90"))
              expect_equal(woce[["salinityUnit"]], list(unit=expression(), scale="PSS-78"))
              expect_equal(woce[["oxygenUnit"]], list(unit=expression(mu*mol/kg), scale=""))
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
                  ##> cat(file, "\n")
                  if (file == "18HU20130507_00235_00001_ct1.csv")
                      d <- expect_warning(read.oce(paste("local_data", file, sep="/")),
                                          "missingValue inferred as -999 from S and T minima")
                  else
                      d <- read.oce(paste("local_data", file, sep="/"))
                  ##> ## summarizing and plotting can depend on the data, so try both
                  ##> summary(d)
                  ##> plot(d)
              }
          }
})


test_that("a broken ODF file that has theta but no S", {
          if (file.exists("local_data/CTD_98911_1P_1_DN.txt")) {
              ## FIXME: why does this yield a warning, but *only* if run in a test suite?
              ##expect_warning(d <- read.oce("local_data/CTD_98911_1P_1_DN.txt"), "NAs introduced by coercion")
              d <- read.oce("local_data/CTD_98911_1P_1_DN.txt")

              ## 1. test access
              expect_equal(length(d[["theta"]]), 127)
              expect_equal(head(d[['theta']]), c(0.0346, 0.1563, 0.2153, 0.1970, 0.1916, 0.2141))

              ## 2. test assignment
              d[["theta"]] <- seq_along(d[["pressure"]])
              expect_equal(length(d[["theta"]]), 127)
              expect_equal(head(d[['theta']]), 1:6)
          }
})

test_that("autoconverts pressure in PSI to in dbar", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              ## test creation of pressure [dbar] from pressure [PSI], using
              ## the constructed file ctd_with_psi.cnv (in which the pressure column
              ## was calculated and inserted into the file, and in which also the
              ## header line was changed to say that pressure is in English units.
              d1 <- expect_warning(read.oce("local_data/ctd.cnv"), "this CNV file has temperature in the IPTS-68 scale")
              d2 <- expect_warning(read.oce("local_data/ctd_with_psi.cnv"), "created 'pressure' from 'pressurePSI'")
              ## use 1e-5 to reflect the number of digits I was using in creating
              ## and then cut/pasting the fake data
              expect_equal(d1[["pressure"]], d2[["pressure"]], tolerance=1e-5)
          }
})
