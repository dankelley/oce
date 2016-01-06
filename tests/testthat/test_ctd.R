## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(ctd)

context("CTD")

test_that("data(ctd) has proper units", {
          expect_equal(ctd[["temperatureUnit"]], c("\u00B0C", "ITS-90"))
          expect_equal(ctd[["conductivityUnit"]], "ratio")
          expect_equal(ctd[["pressureUnit"]], "dbar")
          expect_equal(ctd[["pressureType"]], "sea")
})


test_that("as.ctd() with specified arguments", {
          ctd_ctd <- as.ctd(salinity=ctd[["salinity"]], temperature=ctd[["temperature"]], pressure=ctd[["pressure"]])
          expect_equal(ctd[["salinity"]], ctd_ctd[["salinity"]])
          expect_equal(ctd[["temperature"]], ctd_ctd[["temperature"]])
          expect_equal(ctd[["pressure"]], ctd_ctd[["pressure"]])
          expect_equal(ctd_ctd[["temperatureUnit"]], c("\u00B0C", "ITS-90"))
          expect_equal(ctd_ctd[["conductivityUnit"]], "ratio")
          expect_equal(ctd_ctd[["pressureType"]], "sea")
})

test_that("as.ctd() with a data frame", {
          ctd_df <- as.ctd(data.frame(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
          expect_equal(ctd[["salinity"]], ctd_df[["salinity"]])
          expect_equal(ctd[["temperature"]], ctd_df[["temperature"]])
          expect_equal(ctd[["pressure"]], ctd_df[["pressure"]])
})

test_that("as.ctd() with a list", {
          ctd_l <- as.ctd(list(pressure=ctd[["pressure"]],temperature=ctd[["temperature"]],salinity=ctd[["salinity"]]))
          expect_equal(ctd[["salinity"]], ctd_l[["salinity"]])
          expect_equal(ctd[["temperature"]], ctd_l[["temperature"]])
          expect_equal(ctd[["pressure"]], ctd_l[["pressure"]])
})

test_that("ctd subsetting and trimming", {
          ## NOTE: this is brittle to changes in data(ctd), but that's a good thing, becausing
          ## changing the dataset should be done only when really necessary, e.g. the July 2015
          ## transition to use ITS-90 based temperature.
          data(ctd)
          scanRange <- range(ctd[['scan']])
          newScanRange <- c(scanRange[1] + 20, scanRange[2] - 20)
          ctdTrimmed <- ctdTrim(ctd, "scan", parameters=newScanRange)
          expect_equal(ctdTrimmed[["scan"]][1:3], c(150,151,152))
          expect_equal(ctdTrimmed[["salinity"]][1:3], c(30.8882,30.9301,30.8928))
          expect_equal(ctdTrimmed[["pressure"]][1:3], c(6.198,6.437,6.770))
          expect_equal(ctdTrimmed[["temperature"]][1:3], c(11.734383747900503536,11.630308725905782907,11.4245581060545475790))
          ## next is form a test for issue 669
          data(ctd)
          n <- length(ctd[["salinity"]])
          set.seed(669)
          lon <- ctd[["longitude"]] + rnorm(n, sd=0.05)
          lat <- ctd[["latitude"]] + rnorm(n, sd=0.05)
          ctdnew <- ctd
          ctdnew@data$longitude <- lon
          ctdnew@data$latitude <- lat
          ctdnewSubset <- subset(ctdnew, 200 <= scan & scan <= 300)
          ctdnewTrim <- ctdTrim(ctdnew, method='scan', parameters = c(200, 300))
          expect_equal(ctdnewSubset[['salinity']], ctdnewTrim[['salinity']])
          expect_equal(ctdnewSubset[['scan']], ctdnewTrim[['scan']])
          expect_equal(length(ctdnewSubset[['scan']]), length(ctdnewSubset[['longitude']]))
})

test_that("alter ctd metadata", {
          ctd[["longitude"]] <- 1
          expect_equal(ctd[["longitude"]], 1)
          ctd[["latitude"]] <- 2
          expect_equal(ctd[["latitude"]], 2)
          ## alter data
          S <- ctd[["sal"]] # tests abbreviations also
          ctd[["salinity"]] <- S + 1
          expect_equal(ctd[["salinity"]], S+1)
          top <- subset(ctd, pressure < 5)
          stopifnot(max(top[['pressure']]) < 5)
})

test_that("gsw calcuations on ctd data", {
          SP <- 35
          t <- 10
          p <- 1000
          lon <- 300
          lat <- 30
          ctd <- as.ctd(SP, t, p, longitude=lon, latitude=lat)
          expect_equal(ctd[["SP"]], SP)
          expect_equal(ctd[["t"]], t)
          expect_equal(ctd[["p"]], p)
          expect_equal(ctd[["SP"]], ctd[["salinity"]])
          expect_equal(ctd[["t"]], ctd[["temperature"]])
          expect_equal(ctd[["p"]], ctd[["pressure"]])
          Sstar <- gsw_Sstar_from_SP(SP, p=p, longitude=lon, latitude=lat)
          expect_equal(Sstar, ctd[["Sstar"]])
          SR <- gsw_SR_from_SP(SP=ctd[["SP"]])
          expect_equal(SR, ctd[["SR"]])
          SA <- gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
          expect_equal(SA, ctd[["SA"]])
          Sstar <- gsw_Sstar_from_SA(SA=SA, p=p, longitude=lon, latitude=lat)
          expect_equal(Sstar, ctd[["Sstar"]])
})

test_that("ability to change conductivity unit", {
          ## These came from issue 731
          data(ctd)
          ctd2 <- ctd
          ctd2@data$conductivity <- swCSTp(ctd2) * 42.914
          ctd2[['conductivityUnit']] <- 'mS/cm'
          expect_equal(swSCTp(ctd2), ctd2[['salinity']], scale=1, tolerance=1e-8) # OK on 64-bit OSX
          ctd3 <- ctd
          ctd3@data$conductivity <- swCSTp(ctd3) * 4.2914
          ctd3[['conductivityUnit']] <- 'S/m'
          expect_equal(swSCTp(ctd3), ctd3[['salinity']], scale=1, tolerance=1e-8) # OK on 64-bit OSX
})

context("Reading ctd files")

## A Dalhousie-produced cnv file.
##
##'** Ship:      Divcom3'
##'** Cruise:    Halifax Harbour'
##'** Station:   Stn 2'
##'** Latitude:  N44 41.056'
##'** Longitude: w63 38.633'
test_that("Dalhousie-produced cnv file", {
          d1 <- read.oce(system.file("extdata", "ctd.cnv", package="oce"))
          expect_equal(d1[["temperatureUnit"]], c("\u00B0C", "ITS-90"))
          expect_equal(d1[["conductivityUnit"]], "ratio")
          expect_equal(d1[["pressureUnit"]], "dbar")
          expect_equal(d1[["pressureType"]], "sea")
          expect_equal(d1[["ship"]], "Divcom3")
          expect_equal(d1[["cruise"]], "Halifax Harbour")
          expect_equal(d1[["station"]], "Stn 2")
          expect_equal(d1[["latitude"]], 44+41.056/60)
          expect_equal(d1[["longitude"]], -(63+38.633/60))
          expect_equal(d1[['salinity']][1:3], c(29.9210, 29.9205, 29.9206))
          expect_equal(d1[['pressure']][1:3], c(1.480, 1.671, 2.052))
          ## check conversion from IPTS-68 to ITS-90 worked
          expect_equal(d1[['temperature68']][1:3], c(14.2245, 14.2299, 14.2285))
          expect_equal(d1[['temperature']][1:3], T90fromT68(c(14.2245, 14.2299, 14.2285)))
})

## A file containing CTD data acquired in the Beaufort Sea in 2003.
## I am not sure if this was a standardized format, but I had to work
## with these data so I added support for it.  The files end in .ctd, 
## but oce.magic() recognizes them from the first line.  Note the trailing
## space in the sample data:
##
##'SHIP = CCGS Louis S St.Laurent '
##'CASTNO = 1 '
##'DATE = 11-Aug-2003 '
##'LATITUDE (N)= 71.391 '
##'LONGITUDE (W)= 134.001 '
test_that("Beaufort sea data I (reading ctd/woce/exchange)", {
          d2 <- read.oce(system.file("extdata", "d200321-001.ctd", package="oce"))
          expect_equal(d2[["temperatureUnit"]], c("\u00B0C", "ITS-90"))
          expect_equal(d2[["conductivityUnit"]], "ratio")
          expect_equal(d2[["pressureUnit"]], "dbar")
          expect_equal(d2[["pressureType"]], "sea")
          expect_equal(d2[["ship"]], "CCGS Louis S St.Laurent")
          expect_equal(d2[["station"]], "1")
          expect_equal(d2[["date"]], as.POSIXct("2003-08-11", tz="UTC"))
          expect_equal(d2[["latitude"]], 71.391)
          expect_equal(d2[["longitude"]], -134.001)
          expect_equal(d2[['pressure']][1:3], 1:3)
          expect_equal(d2[['temperature']][1:3], c(-1.1999, -1.2250, -1.2270))
          expect_equal(d2[['salinity']][1:3], c(28.4279, 27.9823, 28.0095))
})

## A file containing CTD data acquired in the Beaufort Sea in 20l2,
## in standard .cnv format (albeit with a date format that was
## not decoded until I added a new format to decodeTime(). Note
## also that the location is stated differently than in the above
## example.
##
##'** Ship:  CCGS Louis St-Laurent'
##'** Station:   BL1'
##'** Depth (m):  87'
##'* NMEA Latitude = 71 20.70 N'
##'* NMEA Longitude = 151 47.26 W'
##'* NMEA UTC (Time) = Aug 09 2012 06:34:34'
test_that("Beaufort sea data II", {
          d3 <- read.oce(system.file("extdata", "d201211_0011.cnv", package="oce"))
          expect_equal(d3[["temperatureUnit"]], c("\u00B0C", "ITS-90"))
          expect_equal(d3[["conductivityUnit"]], "mS/cm")
          expect_equal(d3[["pressureType"]], "sea")
          expect_equal(d3[["ship"]], "CCGS Louis St-Laurent")
          expect_equal(d3[["station"]], "BL1")
          expect_equal(d3[["date"]], as.POSIXct("2012-08-09 06:34:34", tz="UTC"))
          expect_equal(d3[["waterDepth"]], 87)
          expect_equal(d3[["latitude"]], 71+20.70/60)
          expect_equal(d3[["longitude"]], -(151+47.26/60))
          expect_equal(d3[['pressure']][1:3], c(1,2,3))
          expect_equal(d3[['temperature']][1:3], c(-0.0155,0.0005,0.0092))
          expect_equal(d3[['salinity']][1:3], c(25.1637,25.1964,25.3011))
})

## An ODF file measured aboard CCGS SIGMA T, with 
## Catherine Johnson as chief scientist.
test_that("ODF file", {
          d4 <- read.ctd.odf(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF", package="oce"))
          expect_equal(d4[["temperatureUnit"]], c("\u00B0C", "ITS-90"))
          expect_equal(d4[["conductivityUnit"]], "ratio") # was S/m in the .cnv but ratio in ODF
          expect_equal(d4[["pressureType"]], "sea")
          expect_equal(d4[["ship"]], "CCGS SIGMA T (Call Sign: unknown)")
          expect_equal(d4[["cruise"]], "Scotian Shelf")
          expect_equal(d4[["scientist"]], "Catherine Johnson")
          #expect_null(d4[["waterDepth"]])
          expect_equal(d4[["latitude"]], 44.267500)
          expect_equal(d4[["longitude"]], -63.317500)
          expect_equal(d4[['pressure']][1:3], c(0.5, 1.5, 2.0))
          expect_equal(d4[['temperature']][1:3], c(5.885, 5.9124, 5.9188))
          expect_equal(d4[['salinity']][1:3], c(30.8514,30.8593,30.8596))
          ## there are some flagged data in this file
          expect_equal(d4[['pressure']][which(d4[['flag']]!=0)], c(55.5, 60.5, 61.0 ,71.5))
}) 
