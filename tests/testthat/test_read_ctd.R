## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

library(oce)
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
test_that("Beaufort sea data I", {
          d2 <- read.oce(system.file("extdata", "d200321-001.ctd", package="oce"))
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

## An ODF file
##'  CHIEF_SCIENTIST='Glen Harrison','
##'  PLATFORM='Launch  Sigma-T','
##'  INITIAL_LATITUDE=44.266700,'
##'  INITIAL_LONGITUDE=-63.316700,'
##'  START_DATE='Jan 01/2010','
##'  SOUNDING=161.000000,'
test_that("ODF file", {
          d4 <- read.oce(system.file("extdata", "CTD_BCD2010666_01_01_DN.ODF", package="oce"))
          expect_equal(d4[["ship"]], "Launch  Sigma-T")
          expect_equal(d4[["cruise"]], "Scotian Shelf")
          expect_equal(d4[["scientist"]], "Glen Harrison")
          expect_equal(d4[["waterDepth"]], NA)
          expect_equal(d4[["latitude"]], 44.2667)
          expect_equal(d4[["longitude"]], -63.3167)
          expect_equal(d4[['pressure']][1:3], c(1.0,1.5,2.0))
          expect_equal(d4[['temperature']][1:3], c(3.1800,3.1798,3.1804))
          expect_equal(d4[['salinity']][1:3], c(30.7845,30.7775,30.7735))
}) 
