plot <- FALSE
library(oce)
library(testthat)

topoFile <- download.topo(west=-179.5, east=180, south=-89.5, north=90, 
                          resolution=30, format="netcdf", destdir="~/data/topo")
topoWorldNew <- read.topo(topoFile)
imagep(topoWorldNew)
data(topoWorld)

cat(vectorShow(topoWorld[["longitude"]]))
cat(vectorShow(topoWorld[["latitude"]]))
cat("dim topoWorld: ", paste(dim(topoWorld[["z"]]), collapse=" "), "\n")
cat(vectorShow(topoWorldNew[["longitude"]]))
cat(vectorShow(topoWorldNew[["latitude"]]))
cat("dim topoWorldNew: ", paste(dim(topoWorldNew[["z"]]), collapse=" "), "\n")

cat(vectorShow(topoWorldNew[["longitude"]]))
cat(vectorShow(topoWorldNew[["latitude"]]))

pdf("test.pdf")
par(mar=c(2.5,2.5,1,1))
dlon <- abs(diff(topoWorldNew[["longitude"]][1:2])) / 2
dlat <- abs(diff(topoWorldNew[["latitude"]][1:2])) / 2
contour(topoWorld[['longitude']],topoWorld[['latitude']],topoWorld[['z']],
        level=0,drawlabels=FALSE,asp=1,xlim=c(-75,-53),ylim=c(35,55), lwd=2)
contour(topoWorldNew[['longitude']],topoWorldNew[['latitude']],topoWorldNew[['z']],
        level=0,drawlabels=FALSE,asp=1,add=TRUE,col=2, lwd=2)
contour(topoWorldNew[['longitude']]+dlon,topoWorldNew[['latitude']],topoWorldNew[['z']],
        level=0,drawlabels=FALSE,asp=1,add=TRUE,col=3, lwd=2)
contour(topoWorldNew[['longitude']],topoWorldNew[['latitude']]-dlat,topoWorldNew[['z']],
        level=0,drawlabels=FALSE,asp=1,add=TRUE,col=4, lwd=2)
data(coastlineWorldFine, package="ocedata")
lines(coastlineWorldFine[['longitude']], coastlineWorldFine[['latitude']], col=5)
legend("bottomright", bg='white',
       col=1:5, lwd=1, legend=c("Old topo", "New topo",
                                "New lon+ shifted",
                                "New lat- shifted",
                                "coastline"))
dev.off()


## save(topoWorld, file="topoWorld.rda")
## library(tools)
## tools::resaveRdaFiles("topoWorld.rda")

