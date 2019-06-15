## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)

context("topo")

data(topoWorld)
test_that("topoInterpolate", {
          ## Test for same values after rewriting the C code in C++.
          ## NOTE: this test had to be changed when topoWorld was updated on May 18, 2019.
          expect_equal(topoInterpolate(40:55, -50:-35, topoWorld),
                       c(-4149.50, -4309.75, -3954.50, -3324.75, -2405.25, -1586.25, -2190.00, -2819.50,
                         -3027.00, -3406.50, -3549.00, -3282.75, -3496.00, -3052.00, -2481.25, -3218.25))
})

