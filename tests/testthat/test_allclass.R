## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("allclass")

test_that("general", {
          o <- new("oce")
          expect_equal(c("data", "metadata", "processingLog"), sort(slotNames(o)))
          expect_equal(2, length(o[['processingLog']]))
          expect_null(o[['no_such_thing']])
          summary(o)                   # for codecov test
          show(o)                      # for codecov test
})

