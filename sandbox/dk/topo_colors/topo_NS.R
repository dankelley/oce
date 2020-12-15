## Test code for possible local high-res topo dataset
library(oce)
## source("../../../R/oce.R")
data(coastlineWorldFine, package="ocedata")
topoFile <- download.topo(-67,-59.5,43.3,47.2, resolution=1) # NS
## topoFile <- download.topo(-67,-42,40,54, resolution=8) # nwatl
topo <- read.topo(topoFile)
save(topo, file="topoNS.rda")
system("ls -l topoNS.rda")
load("topoNS.rda")
tools::resaveRdaFiles("topoNS.rda")
system("ls -l topoNS.rda")

H <- quantile(abs(topo[["z"]]), 0.98)
cm <- colormap(H*c(-1, 1), col=function(n) oceColorsGebco(region="both",n=n))#, breaks=201)

if (!interactive())
    png("topo_NS.png", width=7, height=5, unit="in", res=150)
imagep(topo, colormap=cm, xlab="", ylab="", mar=c(2,2,1,1))
lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
#polygon(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]], col="lightgrey")
mtext(system(paste("ls -lh", topoFile), intern=TRUE), cex=0.7)
if (!interactive())
    dev.off()

