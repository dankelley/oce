# Improve section plot re legend and adding new elements.
#
# sandbox/issues/20xx/2009/2009_01.R
#
# https://github.com/dankelley/oce/issues/2009

library(oce)
data(section)
# Focus on a recognizable region (with Gulf Stream)
section <- subset(section, longitude < -70)

par(mfrow=c(1, 3))
layout(matrix(c(1, 1, 2, 3), byrow=TRUE, nrow=2), widths=c(1, 0.9))

plot(section, which="map", span=2000)
legend("bottomleft", legend="TEST") # is this visible?

plot(section, which="temperature", xtype="longitude",
     showBottom=TRUE, ztype="image")
points(-71.5, 2000, col=2, pch=20) # is this visible?
text(-71.5, 2000, "71.5W, 2000 dbar", col=2, pos=1) # is this visible?
legend("bottomleft", legend="TEST") # is this visible?

plot(section, which="temperature", xtype="longitude",
     showBottom=TRUE)
points(-71.5, 2000, col=2, pch=20) # is this visible?
text(-71.5, 2000, "71.5W, 2000 dbar", col=2, pos=1) # is this visible?
legend("bottomleft", legend="TEST") # is this visible?
