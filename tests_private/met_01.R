library(oce)
library(canadaHCD)
library(testthat) 
## devtools::install <- github("gavinsimpson/canadaHCD")

context("met")

test_that("as.met() works", {
          a <- hcd_hourly(6358, 2003, 9)
          MET <- as.met(a)
          MET[["time"]] <- MET[["time"]] + 4 * 3600 # get into UTC
          data(met)
          expect_equal(MET[["time"]], met[["time"]])
          expect_equal(MET[["temperature"]], met[["temperature"]])
          expect_equal(MET[["pressure"]], met[["pressure"]])
          expect_equal(MET[["speed"]], met[["speed"]])
          expect_equal(MET[["direction"]], met[["direction"]])
          expect_equal(MET[["u"]], met[["u"]])
          expect_equal(MET[["v"]], met[["v"]])
})

test_that("download/read/plot works with hourly data", {
          f <- download.met(id=6358, year=2003, month=9, destdir=".")
          m <- read.met(f)
          summary(m)
          plot(m)
})


test_that("download/read/plot works with monthly data", {
          f <- download.met(id=1887, deltat="month", destdir=".")
          m <- read.met(f)
          summary(m)
          plot(m)
})

test_that("read.met() works on some files downloaded in 2009", {
          for (file in list.files(path=".", pattern="eng-hourly.*csv")) {
               d <- read.met(file)
          }
})

