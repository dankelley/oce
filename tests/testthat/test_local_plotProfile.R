library(oce)
context("plotProfile handling of *lim")
## Need to set EOS because we are testing ranges on salinity profiles,
## and these vary according to the EOS.
options(oceEOS="gsw")

## These tests derive from issue 1371.
## https://github.com/dankelley/oce/issues/1371

data(ctd)
ctd <- subset(ctd, pressure < 20)
p <- ctd[["pressure"]]
## The 'oxygen' column will exercise code that recognizes
## certain column names, but the 'fake' one exercise the general
## code. There are also some salinity tests here, although they
## ought to amount to the same as 'oxygen' for tests.
ctd <- oceSetData(ctd, 'oxygen', p, unit ='oxygen_unit')
ctd <- oceSetData(ctd, 'fake', -p, unit ='fake_unit')
plim <- c(100, 0)
xlim <- c(-15, -10)

test_that("Test 1", {
          plotProfile(ctd, xtype='fake')
          expect_equal(par("usr"), c(-20.51304, -0.74796, 20.51304, 0.74796))
})

test_that("Test 2", {
          plotProfile(ctd, xtype='fake', ylim=plim)
          expect_equal(par("usr"), c(-20.51304, -0.74796, 104,-4))
})

test_that("Test 3", {
          plotProfile(ctd, xtype='fake', xlim=xlim)
          expect_equal(par("usr"), c(-15.20000, -9.80000, 20.51304, 0.74796))
})

test_that("Test 4", {
          plotProfile(ctd, xtype='fake', plim=plim, xlim=xlim)
          expect_equal(par("usr"), c(-15.2, -9.8, 104, -4.0))
})

test_that("Test 5", {
          plotProfile(ctd, xtype='fake', ylim=plim, xlim=xlim)
          expect_equal(par("usr"), c(-15.2, -9.8, 104, -4))
})

test_that("Test 6", {
          plotProfile(ctd, xtype='oxygen')
          expect_equal(par("usr"), c(0.74796, 20.51304, 20.51304, 0.74796))
})

test_that("Test 7", {
          plotProfile(ctd, xtype='oxygen', plim=plim)
          expect_equal(par("usr"), c(0.74796, 20.51304, 104, -4))
})

test_that("Test 8", {
          plotProfile(ctd, xtype='oxygen', ylim=plim)
          expect_equal(par("usr"), c(0.74796, 20.51304, 104, -4))
})

test_that("Test 9", {
          plotProfile(ctd, xtype='oxygen', xlim=-rev(xlim))
          expect_equal(par("usr"), c(9.8, 15.2, 20.51304, 0.74796))
})

test_that("Test 10", {
          plotProfile(ctd, xtype='oxygen', plim=plim, xlim=-rev(xlim))
          expect_equal(par("usr"), c(9.8, 15.2, 104, -4))
})

test_that("Test 11", {
          plotProfile(ctd, xtype='oxygen', ylim=plim, xlim=-rev(xlim))
          expect_equal(par("usr"), c(9.8, 15.2, 104, -4))
})

test_that("Test 12: how does plim for salinity", {
          ## Salinity has Slim. See how this interacts with xlim.
          plotProfile(ctd, xtype='salinity', ylim=plim)
          expect_equal(par("usr"), c(29.99855641, 31.61705416, 104, -4))
})

test_that("Test 13: does providing both Slim and ylim work?", {
          plotProfile(ctd, xtype='salinity', ylim=plim, Slim=c(30, 31.0))
          expect_equal(par("usr"), c(29.96, 31.04, 104, -4))
})

test_that("Test 14: does providing both xlim and ylim work?", {
          plotProfile(ctd, xtype='salinity', ylim=plim, xlim=c(30, 31.0))
          expect_equal(par("usr"), c(29.99855641, 31.61705416, 104, -4))
})

test_that("Test 15: does S range narrow when p range is narrowed?", {
          plotProfile(ctd, xtype='salinity', plim=c(8,6))
          expect_equal(par("usr"), c(30.88451211, 31.15545961, 8.08, 5.92))
})

test_that("Test 16: does fake range narrow when p range is narrowed?", {
          plotProfile(ctd, xtype='fake', plim=c(8,6))
          expect_equal(par("usr"), c(-8.86696, -5.00704, 8.08, 5.92))
})
