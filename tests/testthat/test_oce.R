## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
## test oce.R

library(oce)

context("oce")

test_that("as.oce", {
          d <- data.frame(x=seq(0,1,length.out=20), y=seq(10,100,length.out=20))
          dh <- head(d)
          dt <- tail(d)
          ##plotPolar(d$x, d$y)
          ##plotSticks(d$x, d$y, d$x/10, d$y/10)
          da <- oceApprox(d$x, d$y, c(0.4, 0.5, 0.6))
          o <- as.oce(d)
          S <- seq(30,35,length.out=10)
          T <- seq(20,10,length.out=10)
          p <- seq(1,100,length.out=10)
          ctd <- as.oce(list(salinity=S, temperature=T, pressure=p))
          ctd <- as.oce(data.frame(salinity=S, temperature=T, pressure=p))
          cl <- as.oce(data.frame(longitude=c(1,2,1), latitude=c(0,1,0)))
          expect_equal(cl[['longitude']], c(1,2,1))
          expect_equal(cl[['latitude']], c(0,1,0))
})

test_that("head and tail", {
          data(adp)
          h <- head(adp)
          h10 <- head(adp, 10)
          expect_equal(h[["time"]], head(adp[["time"]]))
          expect_equal(dim(h[["v"]]), c(6, 84,  4))
          expect_equal(dim(h10[["v"]]), c(10, 84,  4))
          t <- tail(adp)
          t10 <- tail(adp, 10)
          expect_equal(t[["time"]], tail(adp[["time"]]))
          expect_equal(dim(t[["v"]]), c(6, 84,  4))
          expect_equal(dim(t10[["v"]]), c(10, 84,  4))
})

