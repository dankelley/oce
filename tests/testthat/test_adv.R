## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(adp)

context("ADV")

test_that("as.adv() inserts data properly", {
          data(adv)
          adv2 <- enuToOther(adv) # zero angle: should be same
          expect_equal(adv2[['v']], adv[['v']])
          adv3 <- enuToOther(adv, heading=10)
          expect_false(identical(adv3[['v']],adv[['v']]))
          adv4 <- enuToOther(adv3, heading=-10)
          ## FIXME: why does following fail? (issue 1129)
          ## expect_true(identical(adv4[['v']],adv[['v']]))
})
 
