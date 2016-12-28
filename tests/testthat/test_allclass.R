## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("allclass")

test_that("general", {
          o <- new("oce")
          expect_equal(c("data", "metadata", "processingLog"), sort(slotNames(o)))
          expect_equal(2, length(o[['processingLog']]))
          expect_null(o[['no_such_thing']])
          summary(o)
          show(o)
          plot(o)
          ## subsets of base oce object
          oo <- new("oce")
          oo2 <- oceSetData(oo, "a", 1:10)
          subset(oo2, a < 5)
          ## insert data
          o[['no_such_thing']] <- 0    # for codecov test
          expect_null(o[['no_such_thing']])
          ## ## built-in dataset
          ## data(adp)
          ## summary(adp)                 # for codecov test
          ## show(adp)                    # for codecov test
          ## plot(adp)                    # for codecov test
          ## adp2 <- subset(adp, pressure<10) # for codecov test
          ## data(ctd)
          ## st <- ctd[["sigmaTheta"]]
})


