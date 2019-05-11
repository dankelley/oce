library(oce)
source("../../R/topo.R")
data(topoWorld) # for checking for consistency, after updates

topoFile <- download.topo(west=-179.5, east=180, south=-89.5, north=90,
                          resolution=30, format="netcdf", destdir=".", debug=3)
topoWorldTest <- read.topo(topoFile, debug=5)
str(topoWorldTest@data,1)
expect_true(is.vector(topoWorldTest[["longitude"]]))
expect_true(is.vector(topoWorldTest[["latitude"]]))
cat(vectorShow(topoWorld[["longitude"]]))
cat(vectorShow(topoWorldTest[["longitude"]]))
cat(vectorShow(topoWorld[["latitude"]]))
cat(vectorShow(topoWorldTest[["latitude"]]))

cat("topoWorldTest does NOT equal topoWorld:\n")
## retain the tests, so 'make' breaks and the developer notices
expect_equal(topoWorld[["longitude"]], topoWorldTest[["longitude"]])
expect_equal(topoWorld[["latitude"]], topoWorldTest[["latitude"]])

