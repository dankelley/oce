## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

file <- system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package="oce")
CRATwarning <- "\"conductivity\" \\(code name \"CRAT_01\"\\)" # portion of the warning

test_that("ODF CTD file", {
          d <- expect_warning(read.odf(file), CRATwarning)
          expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
          expect_equal(d[["temperatureUnit"]]$scale, "IPTS-68")
          expect_equal(d[["conductivityUnit"]]$unit, expression(S/m))
          expect_equal(d[["pressureType"]], "sea")
          expect_equal(d[["ship"]], "CCGS SIGMA T (Call Sign: unknown)")
          expect_equal(d[["cruise"]], "Scotian Shelf")
          expect_equal(d[["scientist"]], "Catherine Johnson")
          ## expect_null(d[["waterDepth"]])
          expect_equal(d[["latitude"]], 44.267500)
          expect_equal(d[["longitude"]], -63.317500)
          expect_equal(d[['temperature']][1:3], c(5.883587939, 5.910981364, 5.917379829))
          expect_equal(d[['salinity']][1:3], c(30.8514,30.8593,30.8596))
          ## there are some flagged data in this file
          expect_equal(which(d[["salinityFlag"]]!=1), 121)
          expect_equal(c(110,120,121,142), which(d[["sigmaThetaFlag"]]!=1))
})

test_that("ODF CTD file (not as CTD)", {
          d <- expect_warning(read.odf(file), CRATwarning)
          # Test whether the file marked sigmaTheta as bad, when salinity was
          # bad (line 121)
          badSTp <- which(d[["salinityFlag"]]!=1 | d[["temperatureFlag"]]!=1 | d[["pressureFlag"]]!=1)
          expect_equal(3, d@metadata$flags$sigmaTheta[badSTp])
})

test_that("ODF header", {
          d <- expect_warning(read.odf(file), CRATwarning)
          expect_true(is.list((d[["header"]])))
          expect_equal(length(d[["header"]]), 32)
          d <- expect_warning(read.odf(file, header="character"), CRATwarning)
          expect_true(is.vector(d[["header"]]))
          expect_equal(32, length(grep("^[A-Z].*,$", d[["header"]])))
          expect_true(is.character(d[["header"]]))
          d <- expect_warning(read.odf(file, header="list"), CRATwarning)
          expect_true(is.list(d[["header"]]))
          expect_equal(32, length(d[["header"]]))
          expect_true(is.list(d[["header"]][[1]]))
})


# temperature unit/scale (issue 1601)
# https://github.com/dankelley/oce/issues/1801
test_that("ODF temperature scale, IPTS68 and ITS90", {
          # Get IPTS68 temperature, ITS90, and unit, all needed for tests.
          # Note that all read() calls need to check for a warning that results
          # from an error in this ODF file (and quite a few ODF files, it seems).
          Tref68 <- read.table(file, skip=675, header=FALSE)$V4
          Tref90 <- T90fromT68(Tref68)
          unit <- list(unit=expression(degree*C), scale="IPTS-68")
          # read.odf()
          ctd1 <- expect_warning(read.odf(file), CRATwarning)
          expect_equal(ctd1@metadata$units$temperature, unit)
          expect_equal(ctd1@data$temperature, Tref68)
          expect_equal(ctd1[["temperature"]], Tref90)
          # read.ctd.odf()
          ctd2 <- expect_warning(read.ctd.odf(file), CRATwarning)
          expect_equal(ctd2@metadata$units$temperature, unit)
          expect_equal(ctd2@data$temperature, Tref68)
          expect_equal(ctd2[["temperature"]], Tref90)
          # read.oce()
          ctd3 <- expect_warning(read.oce(file), CRATwarning)
          expect_equal(ctd3@metadata$units$temperature, unit)
          expect_equal(ctd3@data$temperature, Tref68)
          expect_equal(ctd3[["temperature"]], Tref90)
          # as.ctd(read.odf())
          ctd1B <- as.ctd(ctd1)
          expect_equal(ctd1B@metadata$units$temperature, unit)
          expect_equal(ctd1B@data$temperature, Tref68)
          expect_equal(ctd1B[["temperature"]], Tref90)
          # as.ctd(read.ctd.odf())
          ctd2B <- as.ctd(ctd2)
          expect_equal(ctd2B@metadata$units$temperature, unit)
          expect_equal(ctd2B@data$temperature, Tref68)
          expect_equal(ctd2B[["temperature"]], Tref90)
          # as.ctd(read.oce())
          ctd3B <- as.ctd(ctd3)
          expect_equal(ctd3B@metadata$units$temperature, unit)
          expect_equal(ctd3B@data$temperature, Tref68)
          expect_equal(ctd3B[["temperature"]], Tref90)
})

