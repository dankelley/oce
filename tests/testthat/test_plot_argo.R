# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data(argo)

test_that("plot works on indexed subsets", {
    for (which in 1:6) {
        expect_silent(plot(argo[["profile", 1]], which = which)) # failed before fixing issue 1603
        expect_silent(plot(argo[["profile", 1:3]], which = which))
    }
})
