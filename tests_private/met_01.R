## As of 20180405, hcd_hourly() cannot read the env-can data. I sent
## a patch.
library(oce)
library(testthat)

test_that("as.met() works", {
          if (FALSE) {
              if (!require(canadaHCD))
                  devtools::install_github("gavinsimpson/canadaHCD")
              a <- canadaHCD::hcd_hourly(6358, 2003, 9)
              MET <- as.met(a)
              MET[["time"]] <- MET[["time"]] + 4 * 3600 # get into UTC
              data(met)
              testthat::expect_equal(MET[["time"]], met[["time"]])
              testthat::expect_equal(MET[["temperature"]], met[["temperature"]])
              testthat::expect_equal(MET[["pressure"]], met[["pressure"]])
              testthat::expect_equal(MET[["speed"]], met[["speed"]])
              testthat::expect_equal(MET[["direction"]], met[["direction"]])
              testthat::expect_equal(MET[["u"]], met[["u"]])
              testthat::expect_equal(MET[["v"]], met[["v"]])
          }
})

test_that("download/read/plot works with hourly data", {
          f <- download.met(id=6358, year=2003, month=9, destdir=".")
          m <- read.met(f)
          summary(m)
          if (!interactive()) png("met_01_01.png")
          plot(m)
          if (!interactive()) dev.off()
})

test_that("download/read/plot works with monthly data", {
          f <- download.met(id=1887, deltat="month", destdir=".")
          m <- read.met(f)
          summary(m)
          if (!interactive()) png("met_01_02.png")
          plot(m)
          if (!interactive()) dev.off()
})

test_that("read.met() works on some files downloaded in 2009", {
          for (file in list.files(path=".", pattern="eng-hourly.*csv")) {
              d <- read.met(file)
          }
})
