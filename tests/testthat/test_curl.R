library(oce)

context("curl")

test_that("curl 1", {
          lat <- c(85,82.5,80,77.5)
          lon <- c(160,162.5,165,167.5)
          taux <- matrix(c(1,6,9,3,7,11,14,10,4,13,2,12,16,8,15,5), nrow=4, ncol=4, byrow=TRUE)
          tauy <- matrix(c(3,20,1,14,1,19,2,6,21,13,28,16,24,4,15,17), nrow=4, ncol=4, byrow=TRUE)
          C <- curl(u=taux, v=tauy, x=lon, y=lat, geographical=TRUE, method=2)
          expect_equal(C$curl[1,1], -3.338849e-05, tolerance=0.000001e-5, scale=1)
          expect_equal(C$x[1], 161.25, tolerance=0.00001, scale=1)
          expect_equal(C$y[1], 83.75, tolerance=0.00001, scale=1)
})

test_that("curl 2", {
          x <- 1:4
          y <- 1:10
          u <- outer(x, y, function(x,y) y/2)
          v <- outer(x, y, function(x,y) -x/2)
          C1 <- curl(u=u, v=v, x=x, y=y, geographical=FALSE, method=1)
          expect_equal(C1$curl, matrix(-1, nrow=4, ncol=10))
          C2 <- curl(u=u, v=v, x=x, y=y, geographical=FALSE, method=2)
          expect_equal(C2$curl, matrix(-1, nrow=3, ncol=9))
})
