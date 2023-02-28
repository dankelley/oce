# https://github.com/dankelley/oce/issues/2009
library(oce)
data(section)
section <- subset(section, longitude < -70) # increase testing speed
source("~/git/oce/R/section.R")
plot(section, which="temperature", xtype="longitude", showBottom=FALSE)
points(-71.5, 2000, col=2, pch=20) # is this visible?
abline(h=2000, col=2) # is this visible?
legend("bottomleft",legend="LEGEND 1") # is this visible?
cat(vectorShow(par('usr')), file=stderr())
cat(vectorShow(par('mar')), file=stderr())
par(xpd=NA)
legend("bottomleft",legend="LEGEND 2") # is this visible?
box(col=2) # is this on plot edges?

