## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4
library(oce)
data(adp)

context("Nortek AD2CP provisional function read.ad2cp()")

test_that("ad2cp on private file", {
          f <- "~/Dropbox/oce_ad2cp/labtestsig3.ad2cp"
          if (file.exists(f)) {
              d <- read.ad2cp(f, 1, 10, 1)
              expect_equal(head(d$buf, 6), as.raw(c(0xa5, 0x0a, 0xa0, 0x10, 0x86, 0x15)))
              expect_equal(head(d$index, 6), c(10, 5530, 6704, 9254, 10428, 11602))
              expect_equal(head(d$length, 6), c(5510, 1164, 2540, 1164, 1164, 1164))
              expect_equal(head(d$id, 6),  c(160, 21, 22, 21, 21, 21))
          }
})
