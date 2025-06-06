library(oce)
test_that("gps", {
    if (1 == length(list.files(path = ".", pattern = "local_data"))) {
        g <- read.oce("local_data/test_trk2.gpx") # test file from rgdal package
        expect_equal(length(g[["longitude"]]), 27)
        expect_equal(length(g[["latitude"]]), 27)
        gg <- as.gps(g[["longitude"]], g[["latitude"]])
        expect_equal(g[["longitude"]], gg[["longitude"]])
        expect_equal(g[["latitude"]], gg[["latitude"]])
    }
})
