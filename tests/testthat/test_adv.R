## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(adp)

context("ADV")

test_that("enuToOther(adv) test of rotation", {
          data(adv)
          ## 1. test with zero heading,pitch,roll (default)
          adv2 <- enuToOther(adv) # all angles are 0 by default, so result should be same
          expect_equal(adv2[['v']], adv[['v']])
          ## 2. test with heading shift of 10deg
          heading <- 10 # heading shift
          adv3 <- enuToOther(adv, heading=heading)
          expect_false(identical(adv3[['v']], adv[['v']]))
          V <- adv[["v"]][, 1:2]
          theta <- heading * pi / 180
          S <- sin(theta)
          C <- cos(theta)
          rotationMatrix <- matrix(c(C, -S, S, C), byrow=TRUE, nrow=2)
          VR <- V %*% rotationMatrix
          expect_equal(VR[1:5, 1], adv3[["v"]][1:5, 1])
          expect_equal(VR[1:5, 2], adv3[["v"]][1:5, 2])
})

f <- "/data/archive/sleiwex/2008/moorings/m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr"
if (file.exists(f)) {
  test_that("read private Sontek file, with numeric 'to' and 'from'", {
            n <- 500000
            adv <- read.oce(f, from=n+10, to=n+12)
            expect_equal(adv[['time']], as.POSIXct(c("2008-06-26 11:12:47.864",
                                                     "2008-06-26 11:12:48.031", "2008-06-26 11:12:48.198"),
                                              tz="UTC"))
            expect_equal(adv[["v"]], matrix(c(-0.02, -0.018, -0.0374, 0.0824,
                                              0.0492, 0.0798, 0.004, 0.0054,
                                              0.0026), ncol=3))
            expect_equal(adv[["a"]], matrix(as.raw(c(0x77, 0x6a, 0x6f, 0x79,
                                                     0x6d, 0x67, 0x77, 0x74,
                                                     0x67)), ncol=3))
            expect_equal(adv[["q"]], matrix(as.raw(c(0x63, 0x61, 0x63, 0x61, 0x62, 0x62, 0x63, 0x63,
                                                     0x63)), ncol=3))
            expect_equal(adv[["pressure"]], c(6.66560152, 6.66560152, 6.56820440))
            expect_equal(adv[['heading']], c(161.4, 0.0, 0.0))
            expect_equal(adv[['pitch']], c(0, 0, 0))
            expect_equal(adv[['roll']], c(-50, 0, 0))
})
}

f <- "/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec"
if (file.exists(f)) {
  test_that("read private Nortek file, with numeric 'to' and 'from'", {
            d <- read.oce(f, from=1, to=100)
            expect_equal(d[["v"]][1:3,1:3], matrix(c(0.094,  0.603, 0.483,
                                                     -0.706, -0.264, 0.037,
                                                     -0.402, 0.194, 0.173),
                                                 byrow=TRUE, ncol=3))
            expect_equal(as.numeric(d[["time"]][1:3])-1214388002,
                         c(0.000, 0.125, 0.250))
            expect_equal(d[["pressure"]][1:3],
                         c(0.215, 0.205, 0.217))
            expect_equal(d[["transformationMatrix"]],
                         matrix(c(2.69360351562, -2.0759277344, -1.2788085938,
                                  0.08471679688, -7.9997558594, 2.2797851562,
                                  -0.34619140625, -0.3603515625,-0.3254394531),
                                byrow=TRUE, ncol=3))
})
}

