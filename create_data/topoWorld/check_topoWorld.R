## Read world topo dataset at 1/12 deg res, then subsample to 1/12 deg.
## File format described at
## http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/ETOPO5.txt
library(oce)
data(topoWorld)
topoWorldOld <- topoWorld

load("topoWorld.rda")
cat(vectorShow(topoWorld[["longitude"]]))
cat(vectorShow(topoWorldOld[["longitude"]]))
expect_equal(topoWorld[["longitude"]], topoWorldOld[["longitude"]])
expect_equal(topoWorld[["latitude"]], topoWorldOld[["latitude"]])

longitude <- topoWorld[["longitude"]]
latitude <- topoWorld[["latitude"]]
z <- topoWorld[["z"]]
if (!interactive()) pdf("check_topoWorld.pdf")
if (plot) {
    par(mfrow=c(2,1))
    imagep(longitude, latitude, z)
}
if (!interactive()) dev.off()
