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
})

