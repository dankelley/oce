## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("webtide")

test_that("webtide", {
          path <- paste(getOption("webtide"), "/data/nwatl", sep="")
          pattern <- "nwatl.nod_ll"
          if (1 == length(list.files(path=path, pattern=pattern))) {
            a <- webtide("predict", longitude=-63, latitude=44, plot=FALSE)
          }
})
 
