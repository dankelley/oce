## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(adp)

context("ADV")

test_that("enuToOther(adv) test of rotation", {
          data(adv)
          ## 1. test with zero heading,pitch,roll (default)
          adv2 <- enuToOther(adv) # all angles are 0 by default, so result should be same
          expect_equal(adv2[['v']], adv[['v']])
          ## 2. test with heading shift of 10deg
          heading <- 10 # heading shift
          adv3 <- enuToOther(adv, heading=heading)
          expect_false(identical(adv3[['v']],adv[['v']]))
          V <- adv[["v"]][,1:2]
          theta <- heading * pi / 180
          S <- sin(theta)
          C <- cos(theta)
          rotationMatrix <- matrix(c(C,-S,S,C), byrow=TRUE, nrow=2)
          VR <- V %*% rotationMatrix
          expect_true(identical(VR, adv3[["v"]][,1:2]))
})
 
