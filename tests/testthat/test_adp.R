## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
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
          ## Test leftover bins: case 1, with a leftover
          adpAvg <- adpEnsembleAverage(adp, n=4, leftover=TRUE)
          nexpected <- 7               # have 6*4 full bins, plus 1 leftover bin
          expect_equal(length(adpAvg[["time"]]), nexpected)
          expect_equal(tail(adp[["time"]], 1), tail(adpAvg[["time"]], 1))
          expect_equal(dim(adpAvg[["v"]])[1], nexpected)
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["q"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["a"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["g"]]))
          ## Test leftover bins: case 2, with no leftover
          adpAvg <- adpEnsembleAverage(adp, n=4)
          nexpected <- 6               # because 6*4 < 25
          expect_equal(length(adpAvg[["time"]]), nexpected)
          expect_equal(dim(adpAvg[["v"]])[1], nexpected)
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["q"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["a"]]))
          expect_equal(dim(adpAvg[["v"]]), dim(adpAvg[["g"]]))
})


