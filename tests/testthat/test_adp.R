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
          n <- 5
          adpAvg <- adpEnsembleAverage(adp, n=n)
          expect_equal(length(adp[["time"]]), n*length(adpAvg[["time"]]))
          expect_equal(dim(adp[["v"]]), c(n, 1, 1) * dim(adpAvg[["v"]]))
          for (name in names(adp@data)) {
            if (is.vector(adp[[name]]) && "distance" != name) {
              expect_equal(adpAvg[[name]][1], mean(adp[[name]][1:n]))
            }
          }
          expect_equal(adpAvg[["v"]][1,1,1], mean(adp[["v"]][1:n,1,1]))
          expect_equal(adpAvg[["v"]][1,2,1], mean(adp[["v"]][1:n,2,1]))
          expect_equal(adpAvg[["v"]][1,1,2], mean(adp[["v"]][1:n,1,2]))
})


