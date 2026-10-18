library(oce)

test_that("CTD indices select observations and their flags", {
    x <- as.ctd(
        salinity = c(34, 34.2, 34.4, 34.6, 34.8),
        temperature = c(12, 11, 10, 9, 8),
        pressure = c(0, 5, 10, 15, 20),
        station = "synthetic",
        flags = list(salinity = c(1, 1, 2, 1, 1))
    )
    for (indices in list(c(1L, 3L), 3L, c(5L, 3L, 1L), -2L, integer())) {
        y <- subset(x, indices = indices)
        for (name in names(x@data)) {
            expect_equal(y@data[[name]], x@data[[name]][indices])
        }
        expect_equal(y[["salinityFlag"]], x[["salinityFlag"]][indices])
        expect_equal(y[["station"]], "synthetic")
    }
})
