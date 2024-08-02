# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

# Case 1: binX1D

test_that("binApply1D old test", {
    set.seed(123)
    n <- 3
    x <- runif(n)
    f <- x^2
    b <- binApply1D(x, f, xbreaks = seq(0, 1, 0.25), FUN = mean)
    expect_equal(b$xbreaks, c(0, 0.25, 0.5, 0.75, 1))
    expect_equal(b$xmids, c(0.125, 0.375, 0.625, 0.875))
    expect_equal(b$result, c(NA, 0.1249814763, NA, 0.6214249866))
})

test_that("binApply1D() with/without include.lowest, tested against corresponding cut()", {
    # Test against the "binMean1D() with..." test, below.
    x <- 1:5
    f <- x^2
    b <- seq(1, 5, 2)
    binApply1DA <- binApply1D(x, f, b, mean)
    # str(binApply1DA, digits.d=10)
    # List of 3
    #  $ xbreaks: num [1:3] 1 3 5
    #  $ xmids  : num [1:2] 2 4
    #  $ result : num [1:2] 6.833333333 20.833333333
    binApply1DB <- binApply1D(x, f, b, mean, include.lowest = TRUE)
    # str(binApply1DB, digits.d=10)
    # List of 3
    #  $ xbreaks: num [1:3] 1 3 5
    #  $ xmids  : num [1:2] 2 4
    #  $ result : num [1:2] 6.5 16.66666667
    expect_equal(binApply1DA$result, c(mean(f[2:3]), mean(f[4:5])))
    expect_equal(binApply1DB$result, c(mean(f[1:3]), mean(f[4:5])))
})

test_that("binMean1D() with/without include.lowest, tested against corresponding cut()", {
    # see https://github.com/dankelley/oce/issues/2112 and also the test just
    # previous to this one.
    x <- 1:5
    f <- x^2
    b <- seq(1, 5, 2)
    binMean1DA <- binMean1D(x, f, b)
    # str(binMean1DA)
    # List of 4
    #  $ xbreaks: num [1:3] 1 3 5
    #  $ xmids  : num [1:2] 2 4
    #  $ number : int [1:2] 2 2
    #  $ result : num [1:2] 6.5 20.5
    binMean1DB <- binMean1D(x, f, b, include.lowest = TRUE)
    # str(binMean1DB)
    # List of 4
    #  $ xbreaks: num [1:3] 1 3 5
    #  $ xmids  : num [1:2] 2 4
    #  $ number : int [1:2] 3 2
    #  $ result : num [1:2] 4.67 20.5
    expect_equal(binMean1DA$result, c(mean(f[2:3]), mean(f[4:5])))
    expect_equal(binMean1DB$result, c(mean(f[1:3]), mean(f[4:5])))
})

test_that("binMean1D", {
    # set.seed(123)
    # n <- 3
    x <- c(0.2875775201, 0.7883051354, 0.4089769218)
    f <- x^2
    # Expect as follows.
    #
    # number  result
    # 0       NA
    # 2       0.1249814763=mean(c(0.2875775201, 0.4089769218)^2)
    # 0       NA
    # 1       0.6214249865=0.7883051354^2
    m <- binMean1D(x, f, seq(0, 1, .25))
    expect_equal(
        m$xbreaks,
        c(0.00, 0.25, 0.50, 0.75, 1.00)
    )
    expect_equal(
        m$xmids,
        c(0.125, 0.375, 0.625, 0.875)
    )
    expect_equal(
        m$number,
        c(0, 2, 0, 1)
    )
    expect_equal(
        m$result,
        c(NA, 0.1249814763, NA, 0.6214249866)
    )
})

test_that("binApply1D with missing bins", {
    # 'result' should have no NA values
    x <- seq(0, 95)
    xbreaks <- seq(0, 100, 10)
    y <- x
    b <- binApply1D(x, y, xbreaks, mean)
    expect_equal(length(b$xmids), length(b$result))
    # should be one NA at the end of 'result'
    x <- seq(0, 89)
    xbreaks <- seq(0, 100, 10)
    y <- x
    b <- binApply1D(x, y, xbreaks, mean)
    expect_equal(length(b$xmids), length(b$result))
    expect_true(is.na(tail(b$result, 1)))
    # should be an NA at both start and end of 'result'
    x <- seq(11, 89)
    xbreaks <- seq(0, 100, 10)
    y <- x
    b <- binApply1D(x, y, xbreaks, mean)
    expect_equal(length(b$xmids), length(b$result))
    expect_true(is.na(b$result[1]))
    expect_true(is.na(tail(b$result, 1)))
})

test_that("binAverage", {
    x <- seq(0, 20, 5)
    y <- x^2
    baA <- binAverage(x, y, xmin = 0, xmax = 20, xinc = 10)
    expect_equal(baA$x, c(5, 15))
    expect_equal(baA$y, c(mean(y[2:3]), mean(y[4:5])))
    baB <- binAverage(x, y, xmin = 0, xmax = 20, xinc = 10, include.lowest = TRUE)
    expect_equal(baB$x, c(5, 15))
    expect_equal(baB$y, c(mean(y[1:3]), mean(y[4:5])))
})

test_that("binCount1D", {
    bc1 <- binCount1D(1:20, seq(0, 20, 2))
    expect_equal(bc1$xbreaks, seq(0, 20, 2))
    expect_equal(bc1$xmids, seq(1, 19, 2))
    expect_equal(bc1$number, rep(2, 10))
})

test_that("binCount1D() with/without include.lowest, tested against corresponding cut()", {
    # see https://github.com/dankelley/oce/issues/2111
    x <- 1:5
    b <- seq(1, 5, 2)
    binCount1DA <- binCount1D(x, b)
    # > binCount1DA
    # $xbreaks
    # [1] 1 3 5
    #
    # $xmids
    # [1] 2 4
    #
    # $number
    # [1] 2 2
    binCount1DB <- binCount1D(x, b, include.lowest = TRUE)
    # > binCount1DB
    # $xbreaks
    # [1] 1 3 5
    #
    # $xmids
    # [1] 2 4
    #
    # $number
    # [1] 3 2
    cutA <- cut(x, b)
    #> cutA
    # [1] <NA>  (1,3] (1,3] (3,5] (3,5]
    # Levels: (1,3] (3,5]
    cutB <- cut(x, b, include.lowest = TRUE)
    # > cutB
    # [1] [1,3] [1,3] [1,3] (3,5] (3,5]
    # Levels: [1,3] (3,5]
    expect_equal(binCount1DA$number, as.integer(table(as.integer(cutA))))
    expect_equal(binCount1DB$number, as.integer(table(as.integer(cutB))))
})

# Case 2: binX2D
test_that("binApply2D", {
    set.seed(123)
    n <- 10
    x <- runif(n)
    y <- runif(n)
    z <- outer(x, y)
    b <- binApply2D(x, y, z, xbreaks = seq(0, 1, 0.25), ybreaks = seq(0, 1, 0.25), FUN = mean, na.rm = TRUE)
    expect_equal(names(b), c("xbreaks", "xmids", "ybreaks", "ymids", "result"))
    # This tests for consistency, as of 2019-Feb-19. Note that there was
    # an error before this time; see https://github.com/dankelley/oce/issues/1493
    # for details.
    expect_equal(
        b$result,
        structure(c(
            NA, NA, 0.27639419053949, 0.479638201680171, NA,
            NA, 0.288604148050149, 0.412574693391033, NA, 0.21404595826396,
            NA, 0.462144185463384, 0.0238428724141042, 0.19474368350674,
            NA, NA
        ), .Dim = c(4L, 4L))
    )
})

#<old>test_that("binCount2D() with include.lowest=FALSE", {
#<old>    # following results checked by eye
#<old>    x <- c(-0.5, -0.5, -2.0, 0.0, 0.0, 2.0, 0.5, -2.0)
#<old>    y <- c(1.0, 0.5, 0.0, -2.0, -0.5, 2.0, 0.5, -2.0)
#<old>    xb <- seq(-2, 2, 1)
#<old>    yb <- seq(-2, 1, 1)
#<old>    bc2 <- binCount2D(x, y, xb, yb)
#<old>    expect_equal(bc2$xbreaks, c(-2, -1, 0, 1, 2))
#<old>    expect_equal(bc2$ybreaks, c(-2, -1, 0, 1))
#<old>    expect_equal(bc2$xmids, c(-1.5, -0.5, 0.5, 1.5))
#<old>    expect_equal(bc2$ymids, c(-1.5, -0.5, 0.5))
#<old>    expect_equal(
#<old>        bc2$number,
#<old>        rbind(
#<old>            c(0, 0, 0),
#<old>            c(0, 1, 2),
#<old>            c(0, 0, 1),
#<old>            c(0, 0, 0)
#<old>        )
#<old>    )
#<old>})

test_that("binMean2D()", {
    # Test updated 2024-03-03 for issue 2199. For a visual
    # test, run the FALSE-ed out block.
    x <- 0.1 + c(-0.5, -0.5, -2.0, 0.0, 0.0, 2.0, 0.5, -2.0)
    y <- 0.1 + c(0.8, 0.1, 0.0, -2.0, -0.9, 2.0, 0.2, -2.0)
    f <- c(1, 2, 3, 4, 5, 6, 7, 8)
    xb <- seq(-2, 2, 1)
    yb <- seq(-2, 2, 1)
    if (FALSE) { # for visual test
        plot(x, y, xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
        text(x + 0.1, y, f)
        abline(v = xb, col = "gray")
        abline(h = yb, col = "gray")
        cex <- 0.8
        col <- 2
        font <- 2
        text(-1.5, -1.5, "n=1 avg=8", col = col, font = font, cex = cex)
        text(-0.5, -1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(0.5, -1.5, "n=1 avg=4", col = col, font = font, cex = cex)
        text(1.5, -1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(-1.5, -0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(-0.5, -0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(0.5, -0.5, "n=1 avg=5", col = col, font = font, cex = cex)
        text(1.5, -0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(-1.5, 0.5, "n=1 avg=3", col = col, font = font, cex = cex)
        text(-0.5, 0.5, "n=2 avg=1.5", col = col, font = font, cex = cex)
        text(0.5, 0.5, "n=1 avg=7", col = col, font = font, cex = cex)
        text(1.5, 0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(-1.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(-0.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(0.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        text(1.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
        rect(-2, -2, 2, 2, border = 2)
    }
    bc2 <- binMean2D(x, y, f, xb, yb)
    expect_equal(bc2$xbreaks, c(-2, -1, 0, 1, 2))
    expect_equal(bc2$ybreaks, c(-2, -1, 0, 1, 2))
    expect_equal(bc2$xmids, c(-1.5, -0.5, 0.5, 1.5))
    expect_equal(bc2$ymids, c(-1.5, -0.5, 0.5, 1.5))
    avg <- rbind(
        c(8, NA, 3, NA),
        c(NA, NA, 1.5, NA),
        c(4, 5, 7, NA),
        c(NA, NA, NA, NA)
    )
    expect_equal(bc2$result, avg)
})

#<old> test_that("binMean2D() with include.lowest=FALSE", {
#<old>     # Test updated 2024-03-03 for issue 2199. For a visual
#<old>     # test, run the FALSE-ed out block.
#<old>     x <- 0.1 + c(-0.5, -0.5, -2.0, 0.0, 0.0, 2.0, 0.5, -2.0)
#<old>     y <- 0.1 + c(0.8, 0.1, 0.0, -2.0, -0.9, 2.0, 0.2, -2.0)
#<old>     f <- c(1, 2, 3, 4, 5, 6, 7, 8)
#<old>     xb <- seq(-2, 2, 1)
#<old>     yb <- seq(-2, 2, 1)
#<old>     if (!FALSE) { # for visual test
#<old>         plot(x, y, xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5))
#<old>         text(x + 0.1, y, f)
#<old>         abline(v = xb, col = "gray")
#<old>         abline(h = yb, col = "gray")
#<old>         cex <- 0.8
#<old>         col <- 2
#<old>         font <- 2
#<old>         text(-1.5, -1.5, "n=1 avg=8", col = col, font = font, cex = cex)
#<old>         text(-0.5, -1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(0.5, -1.5, "n=1 avg=4", col = col, font = font, cex = cex)
#<old>         text(1.5, -1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(-1.5, -0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(-0.5, -0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(0.5, -0.5, "n=1 avg=5", col = col, font = font, cex = cex)
#<old>         text(1.5, -0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(-1.5, 0.5, "n=1 avg=3", col = col, font = font, cex = cex)
#<old>         text(-0.5, 0.5, "n=2 avg=1.5", col = col, font = font, cex = cex)
#<old>         text(0.5, 0.5, "n=1 avg=7", col = col, font = font, cex = cex)
#<old>         text(1.5, 0.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(-1.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(-0.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(0.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         text(1.5, 1.5, "n=0 avg=NA", col = col, font = font, cex = cex)
#<old>         rect(-2, -2, 2, 2, border = 2)
#<old>     }
#<old>     bc2 <- binMean2D(x, y, f, xb, yb)
#<old>     expect_equal(bc2$xbreaks, c(-2, -1, 0, 1, 2))
#<old>     expect_equal(bc2$ybreaks, c(-2, -1, 0, 1, 2))
#<old>     expect_equal(bc2$xmids, c(-1.5, -0.5, 0.5, 1.5))
#<old>     expect_equal(bc2$ymids, c(-1.5, -0.5, 0.5, 1.5))
#<old>     avg <- rbind(
#<old>         c(8, NA, 3, NA),
#<old>         c(NA, NA, 1.5, NA),
#<old>         c(4, 5, 7, NA),
#<old>         c(NA, NA, NA, NA)
#<old>     )
#<old>     expect_equal(bc2$result, avg)
#<old> })
#<old>
#<old> test_that("binMean2D() with include.lowest=TRUE", {
#<old>     # following results checked by eye
#<old>     x <- c(-0.5, -0.5, -2.0, 0.0, 0.0, 2.0, 0.5, -2.0)
#<old>     y <- c(1.0, 0.5, 0.0, -2.0, -0.5, 2.0, 0.5, -2.0)
#<old>     f <- c(1, 2, 3, 4, 5, 6, 7, 8)
#<old>     xb <- seq(-2, 2, 1)
#<old>     yb <- seq(-2, 1, 1)
#<old>     bc2 <- binMean2D(x, y, f, xb, yb, include.lowest = TRUE)
#<old>     expect_equal(bc2$xbreaks, c(-2, -1, 0, 1, 2))
#<old>     expect_equal(bc2$ybreaks, c(-2, -1, 0, 1))
#<old>     expect_equal(bc2$xmids, c(-1.5, -0.5, 0.5, 1.5))
#<old>     expect_equal(bc2$ymids, c(-1.5, -0.5, 0.5))
#<old>     expect_equal(
#<old>         bc2$result,
#<old>         rbind(
#<old>             c(8, 3, NA),
#<old>             c(4, 5, 1.5),
#<old>             c(NA, NA, 7.0),
#<old>             c(NA, NA, NA)
#<old>         )
#<old>     )
#<old> })
