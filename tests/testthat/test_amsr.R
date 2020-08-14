library(oce)

context("amsr")

## library(dc) # not on cran yet
## f1 <- dc.amsr(2016, 08, 09, destdir="~/data/amsr")
## f2 <- dc.amsr(2016, 08, 09, destdir="~/data/amsr")

test_that("amsr[['SSST']]", {
          f <- "~/data/amsr/f34_20200809v8.gz"
          if (file.exists(f)) {
              a <- read.amsr(f)
              SST <- a[["SST"]]
              expect_equal(dim(SST), c(1440, 720))
              expect_equal(mean(SST,na.rm=TRUE), 17.2814401193)
              expect_equal(mean(SST[1,],na.rm=TRUE), 19.2450372208)
              expect_equal(mean(SST[10,],na.rm=TRUE), 18.3939058172)
              expect_equal(SST[500,500], 27.75)
          } else {
              expect_equal(1, 1) ## prevent a NOTE on an empty test
          }
})

test_that("composite amsr", {
          f1 <- "~/data/amsr/f34_20200809v8.gz"
          f2 <- "~/data/amsr/f34_20200810v8.gz"
          if (file.exists(f1) && file.exists(f2)) {
              a1 <- read.amsr(f1)
              a2 <- read.amsr(f2)
              a12 <- composite(a1, a2)
              SST <- a12[["SST"]]
              expect_equal(dim(SST), c(1440, 720))
              expect_equal(mean(SST,na.rm=TRUE), 18.1518488286)
              expect_equal(mean(SST[1,],na.rm=TRUE), 19.1670190275)
              expect_equal(mean(SST[10,],na.rm=TRUE), 19.6761663286)
              expect_equal(SST[200,200], 12.15)
          } else {
              expect_equal(1, 1) ## prevent a NOTE on an empty test
          }
})

test_that("subset(amsr)", {
          f <- "~/data/amsr/f34_20200809v8.gz"
          if (file.exists(f)) {
              a <- read.amsr(f)
              W <- -80
              E <- -40
              S <- 30
              N <- 60
              alon <- subset(a, W < longitude & longitude < E)
              expect_equal(dim(alon[["SST"]]), c(160, 720))
              alat <- subset(a, S < latitude  &  latitude < N)
              expect_equal(dim(alat[["SST"]]), c(1440, 120))
          } else {
              expect_equal(1, 1) ## prevent a NOTE on an empty test
          }
})

