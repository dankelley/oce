## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(adp)

context("tidem")

test_that("tidem", {
          data(sealevel)
          m <- tidem(sealevel)
          summary(m)
          summary(m, p=0.05)
          summary(m, constituent="M2")
          summary(m, constituent=c("M2", "S2"))
          plot(m)
})
 
