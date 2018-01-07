## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
context("Convert rsk to ctd")
data(rsk)
test_that("as.ctd(rsk)", {
          ctd <- as.ctd(rsk)
          ctd[['pressure']] - rsk[['pressure']]
          expect_equal(ctd[["pressure"]]+10.1325, rsk[["pressure"]])
})

test_that("as.ctd(rsk, pressureAtmospheric=1)", {
          ctd <- as.ctd(rsk, pressureAtmospheric=1)
          expect_equal(ctd[["pressure"]]+1, rsk[["pressure"]]-10.1325)
})

test_that("read.rsk()", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
            rsk <- read.oce("local_data/060130_20150904_1159.rsk")
            expect_equal(rsk[["model"]], "RBRconcerto")
            expect_equal(rsk[["pressureType"]], "absolute")
            expect_equal(rsk[["pressureAtmospheric"]], 10.1325)
            expect_equal(rsk[["serialNumber"]], 60130)
            expect_equal(rsk[["sampleInterval"]], 0.167)
            expect_equal(rsk[["rskVersion"]], c(1, 9, 0))
            expect_equal(rsk[["ruskinVersion"]], c(1, 10, 0))
            expect_equal(sort(names(rsk[["data"]])), c("conductivity","pressure","temperature","time"))
          }
})
