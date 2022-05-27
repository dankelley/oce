library(oce)
library(testthat)
load("section.rda")

# Ensure that individual station metadata$stn has length 1.
# https://github.com/dankelley/oce/issues/1908
expect_equal(1L, length(section@data$station[[100]]@metadata$stn))

# Ensure that unit is dbar, not db; see issue 1907 at
# https://github.com/dankelley/oce/issues/1907.
expect_equal(expression(dbar),
    section[["station",100]]@metadata$units$pressure$unit)

n <- length(section[["station",1]]@data)
expect_equal(n, 9) # mainly because we have a 3x3 plot setup
names <- names(section[["station",1]]@data)

if (!interactive()) png("check_station_%02d.png")
par(mfrow=c(3,3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
for (name in names) {
    hist(section[[name]], main="", xlab=name) # a check on issue 983
}

plot(section)
plot(section, xtype="time")
plot(section, xtype="longitude")
plot(section, xtype="track")

if (!interactive()) dev.off()
