library(oce)
if (dir.exists("local_data")) {
    test_that("BOTTLE type", {
        s <- read.oce("local_data/ctd/77DN20020420_hy1.csv")
        expect_true(inherits(s, "section"))
        expect_equal(92, length(s[["station"]]))
        expect_equal(s[["station",1]][["latitude"]], 77.1695)
        expect_equal(s[["station",1]][["longitude"]], 19.3618)
        expect_equal(s[["station",90]][["station"]], "94")
})}

