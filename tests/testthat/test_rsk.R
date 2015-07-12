## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
context("reading rsk files")
file <- system.file("extdata", "sample.rsk.gz", package="oce")

## There are 6 tests in all, stemming from 3 choices for read.rsk() and 2
## choices for as.ctd().  With read.rsk(), patm can be (I) TRUE, (II) FALSE, or
## (III) a number.  The 2 options for as.ctd() are (a) pressureAtmospheric not
## supplied (i.e. NA) and (b) pressureAtmospheric a number.  Thus we have cases
## I.a, I.b, II.a, II.b, III.a and III.b below.
tmp <- tempfile(fileext=".rsk")
if (requireNamespace("R.utils")) {
  R.utils::decompressFile(file, destname=tmp, ext="gz", FUN=gzfile, remove=FALSE)
  test_that("I.a -- as.ctd(read.rsk(..., patm=TRUE))", {
            rsk <- read.rsk(tmp, patm=TRUE)
            ctd <- as.ctd(rsk)
            expect_equal(ctd[["pressure"]], rsk[["pressure"]])})

  test_that("I.b -- as.ctd(read.rsk(..., patm=TRUE), pressureAtmospheric=1)", {
            rsk <- read.rsk(tmp, patm=TRUE)
            ctd <- as.ctd(rsk, pressureAtmospheric=1)
            expect_equal(ctd[["pressure"]], rsk[["pressure"]]-1)})

  test_that("II.a -- as.ctd(read.rsk(..., patm=FALSE))", {
            rsk <- read.rsk(tmp, patm=FALSE)
            ctd <- as.ctd(rsk)
            expect_equal(ctd[["pressure"]], rsk[["pressure"]]-10.1325)})

  test_that("II.b -- as.ctd(read.rsk(..., patm=FALSE), pressureAtmospheric=1)", {
            rsk <- read.rsk(tmp, patm=FALSE)
            ctd <- as.ctd(rsk, pressureAtmospheric=1)
            expect_equal(ctd[["pressure"]], rsk[["pressure"]]-10.1325-1)})

  test_that("III.a -- as.ctd(read.rsk(..., patm=10))", {
            rsk <- read.rsk(tmp, patm=10)
            ctd <- as.ctd(rsk)
            expect_equal(ctd[["pressure"]], rsk[["pressure"]])})

  test_that("III.a -- as.ctd(read.rsk(..., patm=10), pressureAtmospheric=1)", {
            rsk <- read.rsk(tmp, patm=10)
            ctd <- as.ctd(rsk, pressureAtmospheric=1)
            expect_equal(ctd[["pressure"]], rsk[["pressure"]]-1)})
}
