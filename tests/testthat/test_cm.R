# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

test_that("as.cm() works with vectors", {
    n <- 10
    seconds <- seq(0, 100, length.out=n)
    t <- Sys.time() + seconds
    u <- sin(2*pi*seconds) + rnorm(n)
    v <- cos(2*pi*seconds) + rnorm(n)
    p <- 100 + rnorm(n)
    expect_silent(CM <- as.cm(t, u, v, p))
    expect_equal(n, length(CM[["u"]]))
})

test_that("as.cm() works with adv object", {
    data("adv")
    expect_silent(M <- as.cm(adv))
})

test_that("plot,cm-method() works", {
    data(cm)
    expect_silent(plot(cm))
})

test_that("sumamry,cm-method() works", {
    data(cm)
    expect_output(summary(cm))
})

