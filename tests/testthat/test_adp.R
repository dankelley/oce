## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(adp)

context("ADP")

test_that("as.adp() inserts data properly", {
          data(adp)
          t <- adp[["time"]]
          d <- adp[["distance"]]
          v <- adp[["v"]]
          a <- as.adp(time=t, distance=d, v=v)
          expect_equal(a[["time"]], adp[["time"]])
          expect_equal(a[["distance"]], adp[["distance"]])
          expect_equal(a[["v"]], adp[["v"]])
})
 
