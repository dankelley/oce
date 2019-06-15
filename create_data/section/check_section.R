library(oce)
load("section.rda")

if (!interactive()) png("check_station_%02d.png")
n <- length(section[['station',1]]@data)
expect_equal(n, 9) # mainly because we have a 3x3 plot setup
names <- names(section[['station',1]]@data)
par(mfrow=c(3,3), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
for (name in names) {
    hist(section[[name]], main="", xlab=name) # a check on issue 983
}

plot(section)
plot(section, xtype="time")
plot(section, xtype="longitude")
plot(section, xtype="track")

if (!interactive()) dev.off()
