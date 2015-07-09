## The fact that these tests are done during the checking process
## gives at least some assurance that any changes to the code
## have not broken the decoding of these formats.

## Lines within the headers are indicated by '' here.

library(oce)

## A Dalhousie-produced cnv file.
##
##'** Ship:      Divcom3'
##'** Cruise:    Halifax Harbour'
##'** Station:   Stn 2'
##'** Latitude:  N44 41.056'
##'** Longitude: w63 38.633'
##
d1 <- read.oce(system.file("extdata", "ctd.cnv", package="oce"))
stopifnot(d1[["ship"]] == "Divcom3")
stopifnot(d1[["cruise"]] == "Halifax Harbour")
stopifnot(d1[["station"]] == "Stn 2")
stopifnot(all.equal.numeric(d1[["latitude"]], 44.68427, tolerance=0.0001))
stopifnot(all.equal.numeric(d1[["longitude"]], -63.64388, tolerance=0.0001))
stopifnot(all.equal(d1[['salinity']][1:3], c(29.9210, 29.9205, 29.9206)))
stopifnot(all.equal(d1[['pressure']][1:3], c(1.480, 1.671, 2.052)))
## check conversion from IPTS-68 to ITS-90 worked
stopifnot(all.equal(d1[['temperature68']][1:3], c(14.2245, 14.2299, 14.2285)))
stopifnot(all.equal(d1[['temperature']][1:3], T90fromT68(c(14.2245, 14.2299, 14.2285))))

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
##
d2 <- read.oce(system.file("extdata", "d200321-001.ctd", package="oce"))
stopifnot(d2[["ship"]] == "CCGS Louis S St.Laurent")
stopifnot(d2[["station"]] == "1")
stopifnot(all.equal.numeric(d2[["date"]], as.POSIXct("2003-08-11", tz="UTC"), tolerance=0.01))
stopifnot(all.equal.numeric(d2[["latitude"]], 71.391, tolerance=0.0001))
stopifnot(all.equal.numeric(d2[["longitude"]], -134.001, tolerance=0.0001))
stopifnot(all.equal.numeric(d1[['pressure']][1:3], c(1.480,1.671,2.052), tolerance=0.001))
stopifnot(all.equal.numeric(d1[['temperature']][1:3], c(14.2245,14.2299,14.2285), tolerance=0.001))
stopifnot(all.equal.numeric(d1[['salinity']][1:3], c(29.9210,29.9205,29.9206), tolerance=0.001))


## A file containing CTD data acquired in the Beaufort Sea in 20l2,
## in standard .cnv format (albeit with a date format that was
## not decoded until I added a new format to decodeTime().
##'** Ship:  CCGS Louis St-Laurent'
##'** Station:   BL1'
##'** Depth (m):  87'
##'* NMEA Latitude = 71 20.70 N'
##'* NMEA Longitude = 151 47.26 W'
##'* NMEA UTC (Time) = Aug 09 2012 06:34:34'
d3 <- read.oce(system.file("extdata", "d201211_0011.cnv", package="oce"))
stopifnot(d3[["ship"]] == "CCGS Louis St-Laurent")
stopifnot(d3[["station"]] == "BL1")
stopifnot(all.equal.numeric(d3[["date"]], as.POSIXct("2012-08-09 06:34:34", tz="UTC"), tolerance=0.01))
stopifnot(all.equal.numeric(d3[["waterDepth"]], 87, tolerance=0.1))
stopifnot(all.equal.numeric(d3[["latitude"]], 71+20.70/60, tolerance=0.0001))
stopifnot(all.equal.numeric(d3[["longitude"]], -(151+47.26/60), tolerance=0.0001))
stopifnot(all.equal.numeric(d3[['pressure']][1:3], c(1,2,3), tolerance=0.001))
stopifnot(all.equal.numeric(d3[['temperature']][1:3], c(-0.0155,0.0005,0.0092), tolerance=0.001))
stopifnot(all.equal.numeric(d3[['salinity']][1:3], c(25.1637,25.1964,25.3011), tolerance=0.001))

## An ODF file
##'  CHIEF_SCIENTIST='Glen Harrison','
##'  PLATFORM='Launch  Sigma-T','
##'  INITIAL_LATITUDE=44.266700,'
##'  INITIAL_LONGITUDE=-63.316700,'
##'  START_DATE='Jan 01/2010','
##'  SOUNDING=161.000000,'
d4 <- read.oce(system.file("extdata", "CTD_BCD2010666_01_01_DN.ODF", package="oce"), debug=3)
stopifnot(d4[["ship"]] == "Launch  Sigma-T")
stopifnot(d4[["cruise"]] == "Scotian Shelf")
stopifnot(d4[["scientist"]] == "Glen Harrison")
## There is no water depth in this file
stopifnot(all.equal.numeric(d4[["latitude"]], 44.2667, tolerance=0.0001))
stopifnot(all.equal.numeric(d4[["longitude"]], -63.3167, tolerance=0.0001))
stopifnot(all.equal.numeric(d4[['pressure']][1:3], c(1.0,1.5,2.0), tolerance=0.001))
stopifnot(all.equal.numeric(d4[['temperature']][1:3], c(3.1800,3.1798,3.1804), tolerance=0.001))
stopifnot(all.equal.numeric(d4[['salinity']][1:3], c(30.7845,30.7775,30.7735), tolerance=0.001))
 
