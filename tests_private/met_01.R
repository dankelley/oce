library(oce)
library(canadaHCD)
library(testthat) 
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


