## The fact that these tests are done during the checking process
## gives at least some assurance that any changes to the code
## have not broken the decoding of these formats.

library(oce)

## A Dalhousie-produced cnv file
##    ** Station:   Stn 2
##    ** Latitude:  N44 41.056
##    ** Longitude: w63 38.633
d1 <- read.oce(system.file("extdata", "ctd.cnv", package="oce"))
stopifnot(all.equal.numeric(d1[["latitude"]], 44.68427, 0.0001))
stopifnot(all.equal.numeric(d1[["longitude"]], -63.64388, 0.0001))


## A file used in CTD data in the Beaufort Sea in 2003; I am
## not sure if this was a standardized format, but I had to work
## with these data so I coded it in.  The files end in .ctd, 
## but oceMagic() recognizes them from the first line; see
## help(read.ctd) for more.
##   DATE = 11-Aug-2003 
##   LATITUDE (N)= 71.391 
##   LONGITUDE (W)= 134.001 
d2 <- read.oce(system.file("extdata", "d200321-001.ctd", package="oce"))
stopifnot(all.equal.numeric(d2[["date"]], as.POSIXct("2003-08-11", tz="UTC"), 0.01))
stopifnot(all.equal.numeric(d2[["latitude"]], 71.391, 0.0001))
stopifnot(all.equal.numeric(d2[["longitude"]], -134.001, 0.0001))

## An ODF file
##   INITIAL_LATITUDE=44.266700,
##   INITIAL_LONGITUDE=-63.316700,
d3 <- read.oce(system.file("extdata", "CTD_BCD2010666_01_01_DN.ODF", package="oce"))
stopifnot(all.equal.numeric(d3[["waterDepth"]], 161, 0.0001)) # "SOUNDING", not "MAX_DEPTH"
stopifnot(all.equal.numeric(d3[["latitude"]], 44.2667, 0.0001))
stopifnot(all.equal.numeric(d3[["longitude"]], -63.3167, 0.0001))
