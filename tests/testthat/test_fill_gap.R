library(oce)
test_that("fillGapMatrix", {
    om <- matrix(1:20, nrow = 5)
    m <- om
    # Can interpolate past across single points
    m[2, 3] <- NA
    m[3, 3] <- NA
    m[4, 2] <- NA
    fgExpected <- om
    expect_equal(fgExpected, fillGapMatrix(m))
    # Can't interpolate across larger groups ...
    m <- matrix(1:20, nrow = 5)
    m[2:3, 2:3] <- NA
    expect_equal(m, fillGapMatrix(m))
    # ... unless we increase span
    expect_equal(om, fillGapMatrix(m, span = 3))
})
