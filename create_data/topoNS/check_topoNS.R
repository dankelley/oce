## A local high-res topo dataset
library(oce)
data(coastlineWorldFine, package="ocedata")
load("topoNS.rda")

H <- quantile(abs(topoNS[["z"]]), 0.98)
cm <- colormap(H*c(-1, 1), col=function(n) oceColorsGebco(region="both",n=n))

if (!interactive())
    png("topo_NS.png", width=7, height=5, unit="in", res=150)
imagep(topoNS, colormap=cm, xlab="", ylab="", mar=c(2,2,1,1))
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
if (!interactive())
    dev.off()

