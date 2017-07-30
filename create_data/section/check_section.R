library(oce)
load("section.rda")

## The below is also in ../../tests/testthat/test_section.R, and it would be
## smart to update both at the same time.

stn2 <- section[['station', 2]]
twos <- rep(2, 16)
## there are no flags on temperature or pressure
expect_equal(stn2@metadata$flags$salinity, c(2,2,2,2,3,3,2,2,3,3,3,3,3,3,2,2))
expect_equal(stn2@metadata$flags$salinityBottle, c(2,3,2,2,2,3,2,2,2,2,2,2,2,2,2,2))
expect_equal(stn2@metadata$flags$oxygen, twos)
expect_equal(stn2@metadata$flags$silicate, twos)
expect_equal(stn2@metadata$flags$nitrite, twos)
expect_equal(stn2@metadata$flags[["NO2+NO3"]], twos)
expect_equal(stn2@metadata$flags$phosphate, c(2,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2))

plot(section, xtype="time")
plot(section, xtype="latitude")
plot(section, xtype="latitude")
plot(section, xtype="track")

