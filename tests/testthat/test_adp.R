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
 
test_that("adpEnsembleAverage() produces correctly-dimensioned results", {
          data(adp)
          adpAvg <- adpEnsembleAverage(adp, n=5)
          expect_equal(length(adp[["distance"]]), length(adpAvg[["distance"]]))
          ## FIXME: uncomment these (or similar) tests here when we settle on what should be going on
          ##>>  expect_equal(length(adp[["time"]]), 5*length(adpAvg[["time"]]))
          ##>>  expect_equal(dim(adp[["v"]]), c(5, 1, 1) * dim(adpAvg[["v"]]))
})


