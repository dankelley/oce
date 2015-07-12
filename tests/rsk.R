library(oce)
file <- system.file("extdata", "sample.rsk.gz", package="oce")

## There are 6 tests in all, stemming from 3 choices for read.rsk() and 2
## choices for as.ctd().  With read.rsk(), patm can be (I) TRUE, (II) FALSE, or
## (III) a number.  The 2 options for as.ctd() are (a) pressureAtmospheric not
## supplied (i.e. NA) and (b) pressureAtmospheric a number.  Thus we have cases
## I.a, I.b, II.a, II.b, III.a and III.b below.
tmp <- tempfile(fileext=".rsk")
if (requireNamespace("R.utils")) {
    R.utils::decompressFile(file, destname=tmp, ext="gz", FUN=gzfile, remove=FALSE)
    ## I.a
    rsk <- read.rsk(tmp, patm=TRUE)
    ctd <- as.ctd(rsk)
    stopifnot(all.equal.numeric(ctd[["pressure"]], rsk[["pressure"]]))
    ## I.b
    rsk <- read.rsk(tmp, patm=TRUE)
    ctd <- as.ctd(rsk, pressureAtmospheric=1)
    stopifnot(all.equal.numeric(ctd[["pressure"]], rsk[["pressure"]]-1))
    ## II.a
    rsk <- read.rsk(tmp, patm=FALSE)
    ctd <- as.ctd(rsk)
    stopifnot(all.equal.numeric(ctd[["pressure"]], rsk[["pressure"]]-10.1325))
    ## II.b
    rsk <- read.rsk(tmp, patm=FALSE)
    ctd <- as.ctd(rsk, pressureAtmospheric=1)
    stopifnot(all.equal.numeric(ctd[["pressure"]], rsk[["pressure"]]-10.1325-1))
    ## III.a
    rsk <- read.rsk(tmp, patm=10)
    ctd <- as.ctd(rsk)
    stopifnot(all.equal.numeric(ctd[["pressure"]], rsk[["pressure"]]))
    ## III.b
    rsk <- read.rsk(tmp, patm=10)
    ctd <- as.ctd(rsk, pressureAtmospheric=1)
    stopifnot(all.equal.numeric(ctd[["pressure"]], rsk[["pressure"]]-1))
}
