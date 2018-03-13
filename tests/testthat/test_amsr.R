library(oce)

context("amsr")

## library(dc) # not on cran yet
## f1 <- dc.amsr(2016, 08, 09, destdir="~/data/amsr")
## f2 <- dc.amsr(2016, 08, 09, destdir="~/data/amsr")

test_that("amsr[['SSST']]", {
          f1 <- "~/data/amsr/f34_20160808v8.gz"
          if (file.exists(f1)) {
              a1 <- read.amsr(f1)
              SST <- a1[["SST"]]
              expect_equal(dim(SST), c(1440, 720))
              expect_equal(mean(SST,na.rm=TRUE), 17.4659207)
              expect_equal(median(SST,na.rm=TRUE), 18.75)
              expect_equal(mean(SST[1,],na.rm=TRUE), 15.3152381)
              expect_equal(mean(SST[10,],na.rm=TRUE), 16.3884507)
              expect_equal(SST[200,200:205], c(10.80, 10.95, 11.10, 11.40, 11.55, 11.40))
          }
})

test_that("composite amsr", {
          f1 <- "~/data/amsr/f34_20160808v8.gz"
          f2 <- "~/data/amsr/f34_20160808v8.gz"
          if (file.exists(f1) && file.exists(f2)) {
              a1 <- read.amsr(f1)
              a2 <- read.amsr(f2)
              a12 <- composite(a1, a2)
              SST <- a12[["SST"]]
              expect_equal(dim(SST), c(1440, 720))
              expect_equal(mean(SST,na.rm=TRUE), 17.4659207)
              expect_equal(median(SST,na.rm=TRUE), 18.75)
              expect_equal(mean(SST[1,],na.rm=TRUE), 15.3152381)
              expect_equal(mean(SST[10,],na.rm=TRUE), 16.3884507)
              expect_equal(SST[200,200:205], c(10.80, 10.95, 11.10, 11.40, 11.55, 11.40))
          }
})
