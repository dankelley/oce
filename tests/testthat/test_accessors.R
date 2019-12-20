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
          data(ctd)
          freezing <- swTFreeze(ctd)
          ctd <- oceSetData(ctd, "freezing", freezing, list(unit=expression(degree*C), scale="ITS-90"))
          feet <- 3.28048 * swDepth(ctd)
          ctd <- oceSetData(ctd, "depthInFeet", feet, expression(feet))
          expect_identical(ctd[["units"]]$depthInFeet, list(unit=expression(feet), scale=""))
          fathoms <- feet / 6
          ctd <- oceSetData(ctd, "depthInFathoms", fathoms, "fathoms")
          expect_identical(ctd[["units"]]$depthInFathoms, list(unit=expression(fathoms), scale=""))
})

test_that("can use original names", {
          data("ctd")
          ## next two tests relate to issues 1460 and 1547
          expect_equal(diff(ctd[["timeS"]]), diff(as.numeric(ctd[["time"]])))
          expect_equal(length(ctd[["time"]]), length(ctd[["pressure"]])) # 1460,1547
          expect_equal(ctd[["pressure"]], ctd[["pr"]])
          expect_equal(ctd[["depth"]], ctd[["depS"]])
          expect_equal(ctd[["temperature"]], T90fromT68(ctd[["t068"]]))
          expect_equal(ctd[["salinity"]], ctd[["sal00"]])
})

test_that("alter ctd profiles within a section", {
          data("section")
          section[["station", 1]][["S2"]] <- 2 * section[["station", 1]][["salinity"]]
          expect_equal(section[["station", 1]][["S2"]],
                       2 * section[["station", 1]][["salinity"]])
})

## Tests with a funky file, from 1154 (we may need to chop these,
## since it does not make sense to use space in a CRAN package
## with a broken file.

## Next tests are in support of issue 1162, relating to the rules for accessing
## derived quantities.
test_that("accessor operations (ctd)", {
          data(ctd)
          S <- ctd[["salinity"]]
          expect_equal(head(S), c(29.9210, 29.9205, 29.9206, 29.9219, 29.9206, 29.9164))
          ctd[["salinity"]] <- S + 0.01
          SS <- ctd[["salinity"]]
          expect_equal(head(SS), 0.01 + c(29.9210, 29.9205, 29.9206, 29.9219, 29.9206, 29.9164))
          ctd[["SS"]] <- SS
          expect_equal(head(ctd[["SS"]]), 0.01 + c(29.9210, 29.9205, 29.9206, 29.9219, 29.9206, 29.9164))
})

## accessors to metadata or data
## https://github.com/dankelley/oce/issues/1554
test_that("accessor operations, specifying data or metadata", {
          data(ctd)
          expect_equal(ctd[["longitude"]], ctd[["longitude", "metadata"]])
          expect_null(ctd[["longitude", "data"]])
          expect_equal(ctd[["temperature"]], ctd[["temperature", "data"]])
          expect_equal(ctd[["salinity"]], ctd[["salinity", "data"]])
          expect_equal(ctd[["salinity"]], ctd[["sal00", "data"]]) # originalName
          ## Now, create something with conflicts
          o <- new("oce")
          o <- oceSetMetadata(o, "foo", "metadataBar")
          o <- oceSetData(o, "foo", "dataBar")
          expect_equal(o[["foo"]], "metadataBar")
          expect_equal(o[["foo", "metadata"]], "metadataBar")
          expect_equal(o[["foo", "data"]], "dataBar")
          expect_error(o[["foo","unknown"]], "second arg must be")
})

test_that("derived quantities handled properly (ctd)", {
          ## do we get the same theta by both methods, if the object lacks
          ## theta at the start?
          data(ctd)
          expect_null(ctd[["noSuchThing"]])
          thetaByAccessor <- ctd[["theta"]]
          thetaByFunction <- swTheta(ctd)
          expect_equal(thetaByAccessor, thetaByFunction)
          ## Now, insert theta into the object, and alter salinity, to
          ## check that [[ gets the object value and swTheta() computes
          ## a new value.
          ctd[["theta"]] <- thetaByAccessor
          expect_equal(ctd[["theta"]], thetaByAccessor)
          expect_equal(ctd[["theta"]], swTheta(ctd))
          ctd[["S"]] <- ctd[["S"]] + 0.01 # alter S
          ## next values are just what I got from these data, i.e. they only
          ## form a consistency check, if the data or if swTheta() ever change.
          expect_equal(head(swTheta(ctd, eos="unesco")),
                       c(14.2208818496, 14.2262540208, 14.2248015615,
                         14.2218758247, 14.2263218577, 14.2328135332))
          ## Try both EOSs; need loction for GSW
          expect_equal(swTheta(ctd, eos="unesco"), swTheta(ctd[["salinity"]],
                                             ctd[["temperature"]],
                                             ctd[["pressure"]], eos="unesco"))
          expect_equal(swTheta(ctd, eos="gsw"), swTheta(ctd[["salinity"]],
                                             ctd[["temperature"]],
                                             ctd[["pressure"]],
                                             longitude=ctd[["longitude"]],
                                             latitude=ctd[["latitude"]], eos="gsw"))
})

test_that("accessor operations (adp)", {
          data(adp)
          v <- adp[["v"]]
          expect_equal(v[1:5,1,1], c(-0.11955770778, -0.09925398341, 0.10203801933,
                                     0.09613003081, 0.24394126236))
          expect_null(adp[["noSuchThing"]])
          adp[["somethingNew"]] <- 1:4
          expect_true("somethingNew" %in% names(adp[["data"]]))
          expect_equal(adp[["somethingNew"]], 1:4)
})

test_that("renaming items in the data slot", {
          data(ctd)
          bad <- oceRenameData(ctd, "salinity", "saltiness", note="a bad idea!")
          expect_error(plot(bad), "data slot lacks 'salinity")
})
