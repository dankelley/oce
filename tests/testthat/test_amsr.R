library(oce)

context("amsr")

test_that("amsr[['SSST']]", {
          data(amsr)
          SST <- amsr[["SST"]]
          expect_equal(dim(SST), c(160, 120))
          expect_equal(mean(SST, na.rm=TRUE), 22.2395922219 )
          expect_equal(SST[1, 1], 31.35)
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
          sub <- subset(amsr,  34 <= latitude & latitude <=  53)
          expect_equal(dim(sub[["SST"]]), c(160, 76))
          sub <- subset(sub,  -75 <= longitude & longitude <= -50)
          expect_equal(dim(sub[["SST"]]), c(100, 76))
})

