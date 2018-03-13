## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)

context("topo")

data(topoWorld)
test_that("topoInterpolate", {
          ## Test for same values after rewriting the C code in C++.
          expect_equal(topoInterpolate(40:55, -50:-35, topoWorld),
                       c(-4102, -4137, -3899, -3623, -2474, -1503, -2192,
                         -2998, -3184, -3025, -3996, -3242, -3266, -2819,
                         -3868, -2497))
})


