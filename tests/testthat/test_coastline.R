# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)

test_that("various methods of coastline creation", {
    lon <- c(1,2,1)
    lat <- c(0,1,0)
    cl1 <- as.oce(list(longitude=lon, latitude=lat))
    cl2 <- as.oce(data.frame(longitude=lon, latitude=lat))
    cl3 <- as.coastline(longitude=lon, latitude=lat)
    cl4 <- new("coastline")
    cl4 <- oceSetData(cl4, "longitude", lon)
    cl4 <- oceSetData(cl4, "latitude", lat)
    expect_equal(cl1[["latitude"]], cl2[["latitude"]])
    expect_equal(cl1[["latitude"]], cl3[["latitude"]])
    expect_equal(cl1[["latitude"]], cl4[["latitude"]])
    expect_equal(cl1[["longitude"]], cl2[["longitude"]])
    expect_equal(cl1[["longitude"]], cl3[["longitude"]])
    expect_equal(cl1[["longitude"]], cl4[["longitude"]])
})

test_that("coastlineCut", {
    data(coastlineWorld)
    expect_silent(coastlineCut(coastlineWorld, lon_0=100))
})

