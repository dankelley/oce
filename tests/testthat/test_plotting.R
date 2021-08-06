library(oce)
test_that("multi-panel plots leave usr as it was originally", {
          skip_on_cran()
          orig <- par('usr')
          data(adp)
          expect_silent(plot(adp))
          expect_equal(par('usr'), orig)
          data(adv)
          expect_silent(plot(adv))
          expect_equal(par('usr'), orig)
          data(cm)
          expect_silent(plot(cm))
          expect_equal(par('usr'), orig)
          data(ctd)
          if (requireNamespace("ocedata", quietly=TRUE)) {
              expect_silent(plot(ctd))
          } else {
              expect_warning(plot(ctd), "CTD plots will have better coastlines after")
          }
          expect_equal(par('usr'), orig)
          data(lisst)
          expect_silent(plot(lisst))
          expect_equal(par('usr'), orig)
          data(lobo)
          expect_silent(plot(lobo))
          expect_equal(par('usr'), orig)
          data(rsk)
          expect_silent(plot(rsk))
          expect_equal(par('usr'), orig)
          data(sealevel)
          expect_silent(plot(sealevel))
          expect_equal(par('usr'), orig)
          data(section)
          expect_silent(plot(section))
          expect_equal(par('usr'), orig)
})


test_that("oce.plot.ts() catches xlim errors", {
          t0 <- as.POSIXct("1967-07-04", tz="UTC") # Canada's centenary
          t <- seq(t0, length.out=24, by="1 hour")
          y <- sin(as.numeric(t - t0) * 2 * pi / (12 * 3600))
          expect_silent(oce.plot.ts(t,y))
          xlim <- range(t)
          expect_silent(oce.plot.ts(t,y, xlim=xlim))
          expect_error(oce.plot.ts(t,y, xlim=rev(xlim)), "the elements of xlim must be in order")
          expect_error(oce.plot.ts(t,y, xlim=c(xlim[1], NA)), "missing value where TRUE/FALSE needed")
})

