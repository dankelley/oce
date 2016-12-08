## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Accessors")

test_that("get/set/delete data", {
          data("ctd")
          S <- oceGetData(ctd, "salinity")
          ctd2 <- oceSetData(ctd, name="fake", value=2*S,
                             unit=list(unit=expression(), scale=""),
                             originalName="FAKE")
          expect_equal(2*S, ctd2[["fake"]])
          expect_equal(S, ctd@data$salinity)
          expect_equal(S, ctd2@data$salinity)
          ## deletion
          d <- oceDeleteData(ctd, "fake")
          expect_false("fake" %in% names(d[["data"]]))
          expect_false("fake" %in% names(d@data))
})

test_that("get/set/delete metadata", {
          data("ctd")
          type <- oceGetMetadata(ctd, name="type")
          expect_equal(type, "SBE")
          ctd2 <- oceSetMetadata(ctd, name="type", value="fake")
          expect_equal(oceGetMetadata(ctd2, "type"), "fake")
          expect_equal(ctd2[["metadata"]]$type, "fake")
          ctd3 <- oceDeleteMetadata(ctd2, "type")
          expect_false("fake" %in% names(ctd3[["metadata"]]))
})


test_that("data", {
          data("ctd")
          d <- oceGetData(ctd, "salinity")
          expect_equal(d, ctd[["salinity"]])
          expect_equal(d, ctd@data$salinity)
})


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


test_that("three methods for specifying units", {
          data("ctd")
          freezing <- swTFreeze(ctd)
          ctd <- oceSetData(ctd, "freezing", freezing, list(unit=expression(degree*C), scale="ITS-90"))
          feet <- 3.28048 * swDepth(ctd)
          ctd <- oceSetData(ctd, "depthInFeet", feet, expression("feet"))
          expect_identical(ctd[["units"]]$depthInFeet, list(unit=expression("feet"), scale=""))
          fathoms <- 0.546807 * swDepth(ctd)
          ctd <- oceSetData(ctd, "depthInFathoms", fathoms, "fathoms")
          expect_identical(ctd[["units"]]$depthInFathoms, list(unit=expression("fathoms"), scale=""))
})

test_that("can use original names", {
          data("ctd")
          expect_equal(ctd[["time"]], ctd[["timeS"]])
          expect_equal(ctd[["pressure"]], ctd[["pr"]])
          expect_equal(ctd[["depth"]], ctd[["depS"]])
          expect_equal(ctd[["temperature"]], T90fromT68(ctd[["t068"]]))
          expect_equal(ctd[["salinity"]], ctd[["sal00"]])
})          

