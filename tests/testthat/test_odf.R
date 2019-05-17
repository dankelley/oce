## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("ODF")

## This ODF file is modified from the one in inst/extdata, by having a sequence
## of distinct NULL_VALUE entries, one per data item, and by having the
## first few data entries modified. This tests the ability to handle
## a different code for each data type.
test_that("ODF CTD file", {
          expect_warning(d <- read.ctd.odf("CTD_BCD2014666_008_1_DN_altered.ODF.gz"),
                         "\"CRAT_01\" should be unitless")
          expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
          expect_equal(d[["temperatureUnit"]]$scale, "IPTS-68")
          ## FIXME: following works manually but fails in Rstudio build
          ## expect_equal(d[["conductivityUnit"]]$unit, expression()) # was S/m in the .cnv but ratio in ODF
          expect_equal(d[["pressureType"]], "sea")
          expect_equal(d[["ship"]], "CCGS SIGMA T (Call Sign: unknown)")
          expect_equal(d[["cruise"]], "Scotian Shelf")
          expect_equal(d[["scientist"]], "Catherine Johnson")
          ## expect_null(d[["waterDepth"]])
          expect_equal(d[["latitude"]], 44.267500)
          expect_equal(d[["longitude"]], -63.317500)
          expect_equal(d[['pressure']][1:3], c(0.5, NA, 2.0)) # the NA was hand-altered
          expect_equal(d[['temperature']][1:3], c(5.883587939, 5.910981364, 5.917379829))
          expect_equal(d[['salinity']][1:3], c(30.8514,30.8593,30.8596))
          ## there are some flagged data in this file
          expect_equal(d[['pressure']][which(d[['QCFlag']]!=0)], c(55.5, 60.5, 61.0 ,71.5))
          ## Finally, check the altered missing-value setup. The file was
          ## hand-altered.
          ## Item               code   index of altered datum
          ## ================== ====   ======================
          ## scan               -990   1
          ## pressure           -991   2
          ## pressureFlag       -992   3
          ## temperature        -993   4
          ## temperatureFlag    -994   5
          ## conductivityFlag   -995   6
          ## conductivityFlag   -996   7
          ## oxygenVoltage      -997   8
          ## oxygenVoltageFlag  -998   9
          ## fluorescence       -999  10
          ## fluorescenceFlag  -1000  11
          ## par               -1001  12
          ## parFlag           -1002  13
          ## salinity          -1003  14
          ## salinityFlag      -1004  15
          ## oxygen            -1005  16
          ## oxygenFlag        -1006  17
          ## sigmaTheta        -1007  18
          ## sigmaThetaFlag    -1008  19
          expect_equal( 1, which(is.na(d[["scan"]])))
          expect_equal( 2, which(is.na(d[["pressure"]])))
          expect_equal( 3, which(is.na(d[["pressureFlag"]])))
          expect_equal( 4, which(is.na(d[["temperature"]])))
          expect_equal( 5, which(is.na(d[["temperatureFlag"]])))
          expect_equal( 6, which(is.na(d[["conductivity"]])))
          expect_equal( 7, which(is.na(d[["conductivityFlag"]])))
          expect_equal( 8, which(is.na(d[["oxygenVoltage"]])))
          expect_equal( 9, which(is.na(d[["oxygenVoltageFlag"]])))
          expect_equal(10, which(is.na(d[["fluorometer"]])))
          expect_equal(11, which(is.na(d[["fluorometerFlag"]])))
          expect_equal(12, which(is.na(d[["par"]])))
          expect_equal(13, which(is.na(d[["parFlag"]])))
          expect_equal(14, which(is.na(d[["salinity"]])))
          expect_equal(15, which(is.na(d[["salinityFlag"]])))
          expect_equal(16, which(is.na(d[["oxygen"]])))
          expect_equal(17, which(is.na(d[["oxygenFlag"]])))
          ## Next will be NA for bad pressure, salinity, temperature, but NOT
          ## for the altered bad sigmaTheta. This is because oce *calculates*
          ## sigmaTheta for the CTD case ... but contrast this with the
          ## next test.
          expect_equal(c(2, 4, 14), which(is.na(d[["sigmaTheta"]]))) # computed
          expect_equal(18, which(is.na(d[["SIGP_01"]]))) # original data
          expect_equal(19, which(is.na(d[["sigmaThetaFlag"]])))

})

test_that("ODF CTD file (not as CTD)", {
          expect_warning(d <- read.odf("CTD_BCD2014666_008_1_DN_altered.ODF.gz"),
                         "\"CRAT_01\" should be unitless")
          ## First, check as in the previous test.
          expect_equal( 1, which(is.na(d[["scan"]])))
          expect_equal( 2, which(is.na(d[["pressure"]])))
          expect_equal( 3, which(is.na(d[["pressureFlag"]])))
          expect_equal( 4, which(is.na(d[["temperature"]])))
          expect_equal( 5, which(is.na(d[["temperatureFlag"]])))
          expect_equal( 6, which(is.na(d[["conductivity"]])))
          expect_equal( 7, which(is.na(d[["conductivityFlag"]])))
          expect_equal( 8, which(is.na(d[["oxygenVoltage"]])))
          expect_equal( 9, which(is.na(d[["oxygenVoltageFlag"]])))
          expect_equal(10, which(is.na(d[["fluorometer"]])))
          expect_equal(11, which(is.na(d[["fluorometerFlag"]])))
          expect_equal(12, which(is.na(d[["par"]])))
          expect_equal(13, which(is.na(d[["parFlag"]])))
          expect_equal(14, which(is.na(d[["salinity"]])))
          expect_equal(15, which(is.na(d[["salinityFlag"]])))
          expect_equal(16, which(is.na(d[["oxygen"]])))
          expect_equal(17, which(is.na(d[["oxygenFlag"]])))
          ## Test sigmaTheta in two ways. Using [["sigmaTheta"]] will *compute*
          ## the value (so it will be NA when S, T or p is NA), but
          ## [["SIGP_01"]] access the *supplied* value, so it is NA at line 18.
          ## NOTE: the behaviour for the [["sigmaTheta"]] case was to return the
          ## same as the [["SIGP_01"]] case until 2019 May 17, when some changes
          ## got made for issue 1554 (see
          ## https://github.com/dankelley/oce/issues/1554), whereupon I realized
          ## that the old behaviour made ODF different from other oce data
          ## types, which is not sensible.
          badSTp <- which(is.na(d@data$salinity)|is.na(d@data$temperature)|is.na(d@data$pressure))
          expect_equal(badSTp, which(is.na(d[["sigmaTheta"]]))) # computed
          expect_equal(18, which(is.na(d[["SIGP_01"]]))) # original data
          expect_equal(19, which(is.na(d[["sigmaThetaFlag"]])))

})

test_that("ODF header", {
          f <- system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package="oce")
          expect_warning(d <- read.odf(f),
                         "\"CRAT_01\" should be unitless")
          expect_null(d[["header"]])
          expect_warning(d <- read.odf(f, header="character"),
                         "\"CRAT_01\" should be unitless")
          expect_true(is.vector(d[["header"]]))
          expect_equal(32, length(grep("^[A-Z].*,$", d[["header"]])))
          expect_true(is.character(d[["header"]]))
          expect_warning(d <- read.odf(f, header="list"),
                         "\"CRAT_01\" should be unitless")
          expect_true(is.list(d[["header"]]))
          expect_equal(32, length(d[["header"]]))
          expect_true(is.list(d[["header"]][[1]]))
})

