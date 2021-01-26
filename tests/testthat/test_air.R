## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)
context("air properties")
test_that("air density at 0C and 100kPa", {
          ## test value from <https://en.wikipedia.org/wiki/Density_of_air>
          expect_equal(1.2754, airRho(0, 1e5), tolerance=0.001)
})


