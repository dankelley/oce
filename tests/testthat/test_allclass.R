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
          expect_warning(plot(o), "no data to plot")
          ## subsets of base oce object
          oo <- new("oce")
          oo2 <- oceSetData(oo, "a", 1:10)
          subset(oo2, a < 5)
          ## insert data
          expect_null(o[['noSuchThing']])
          o[['noSuchThing']] <- 0
          expect_equal(o[['noSuchThing']], 0)
          ## ## built-in dataset
          ## data(adp)
          ## summary(adp)                 # for codecov test
          ## show(adp)                    # for codecov test
          ## plot(adp)                    # for codecov test
          ## adp2 <- subset(adp, pressure<10) # for codecov test
          ## data(ctd)
          ## st <- ctd[["sigmaTheta"]]
})


