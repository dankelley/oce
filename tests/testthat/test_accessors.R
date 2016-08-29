## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Accessors")

test_that("retrieve units", {
          data("ctd")
          ## pre 20160430 expect_equal(ctd[["temperatureUnit"]], list(unit=expression(degree*C), scale="ITS-90"))
          ## pre 20160430 expect_equal(ctd[["temperature unit"]], expression(degree*C))
          ## pre 20160430 expect_equal(ctd[["temperature scale"]], "ITS-90")
          expect_equal(ctd[["temperatureUnit"]], list(unit=expression(degree*C), scale="IPTS-68"))
          expect_equal(ctd[["temperature unit"]], expression(degree*C))
          expect_equal(ctd[["temperature scale"]], "IPTS-68")
          expect_equal(ctd[["pressureUnit"]], list(unit=expression(dbar), scale=""))
          expect_equal(ctd[["pressure unit"]], expression(dbar))
          expect_equal(ctd[["pressure scale"]], "")
})

test_that("alter units", {
          data("ctd")
          ctd[["metadata"]]$units$salinity <- list(unit=expression(foo), scale="bar")
          expect_equal(ctd[["salinityUnit"]], list(unit=expression(foo), scale="bar"))
})

test_that("can use original names", {
          data(ctd)
          expect_equal(ctd[["time"]], ctd[["timeS"]])
          expect_equal(ctd[["pressure"]], ctd[["pr"]])
          expect_equal(ctd[["depth"]], ctd[["depS"]])
          expect_equal(ctd[["temperature"]], T90fromT68(ctd[["t068"]]))
          expect_equal(ctd[["salinity"]], ctd[["sal00"]])
})          

