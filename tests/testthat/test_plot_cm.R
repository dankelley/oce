# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data(cm)

test_that("plot,cm-method() works", {
    expect_silent(plot(cm))
})
