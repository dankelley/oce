## A local high-res topo dataset
library(oce)
# 1 minute resolution is 1.8km north-south, about 1.5km in east-west
topoFile <- download.topo(-67, -59.5, 43.3, 47.2, resolution=1)
topoNS <- read.topo(topoFile)
save(topoNS, file="topoNS.rda", version=2)
system("ls -l topoNS.rda")
load("topoNS.rda")
tools::resaveRdaFiles("topoNS.rda", version=2)
system("ls -l topoNS.rda")

