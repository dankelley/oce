library(oce)
test_that("fillGapMatrix on interior points", {
    om <- matrix(1:20, nrow = 5)
    m <- om
    # We can interpolate past across single gaps
    m[2, 3] <- NA
    m[3, 3] <- NA
    m[4, 2] <- NA
    fgExpected <- om
    expect_equal(fgExpected, fillGapMatrix(m))
    # Can't interpolate across larger groups ...
    m <- matrix(1:20, nrow = 5)
    m[2:3, 2:3] <- NA
    expect_equal(m, fillGapMatrix(m))
    # ... unless we increase the permitted gap
    expect_equal(om, fillGapMatrix(m, fillgap = 2))
})


test_that("fillGapMatrix on boundaries", {
    # Test that a single element on an edge can be filled,
    # but not two adjacent elements.
    m0 <- matrix(1:20, nrow = 4)
    ni <- nrow(m0)
    nj <- ncol(m0)
    # i=1 side
    m <- m0
    m[1, 2] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m0)
    m <- m0
    m[1, 2:3] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)

    # j=1 side
    m <- m0
    m[2, 1] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m0)
    m <- m0
    m[2:3, 1] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)

    # i=ni side
    m <- m0
    m[ni, 2] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m0)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)
    m <- m0
    m[ni, 2:3] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)

    # j=nj side
    m <- m0
    m[2, nj] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m0)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)
    m <- m0
    m[2:3, nj] <- NA
    mf <- fillGapMatrix(m)
    expect_equal(mf, m)
    mf <- fillGapMatrix(m, fillgap = 2)
    expect_equal(mf, m0)
})
