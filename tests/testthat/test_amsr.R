library(oce)

context("amsr")

test_that("amsr[['SSST']]", {
          data(amsr)
          SST <- amsr[["SST"]]
          expect_equal(dim(SST), c(44, 36))
          expect_equal(mean(SST, na.rm=TRUE), 25.96231)
          expect_equal(SST[1, 1], 30.00)
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
          data(amsr)
          sub <- subset(amsr,  37 <= latitude & latitude <=  39)
          expect_equal(dim(sub[["SST"]]), c(44, 8))
          sub <- subset(sub,  -68 <= longitude & longitude <= -65)
          expect_equal(dim(sub[["SST"]]), c(12, 8))
})

