data(rsk)
test_that("as.ctd(rsk)", {
    ctd <- as.ctd(rsk)
    ctd[["pressure"]] - rsk[["pressure"]]
    expect_equal(ctd[["pressure"]] + 10.1325, rsk[["pressure"]])
})

test_that("as.ctd(rsk, pressureAtmospheric=1)", {
    ctd <- as.ctd(rsk, pressureAtmospheric = 1)
    expect_equal(ctd[["pressure"]] + 1, rsk[["pressure"]] - 10.1325)
})

if (requireNamespace("RSQLite", quietly = TRUE)) {
    test_that("read.rsk() patm handling, with a local file", {
        ## Actually, this should work with any rsk file.
        f <- "local_data/060130_20150904_1159.rsk"
        if (file.exists(f)) {
            a <- read.oce(f)
            b <- read.oce(f, patm = FALSE)
            expect_equal(a[["pressure"]], b[["pressure"]])
            c <- read.oce(f, patm = TRUE)
            expect_equal(a[["pressure"]] - 10.1325, c[["pressure"]])
            d <- read.oce(f, patm = 10)
            expect_equal(a[["pressure"]] - 10, d[["pressure"]])
        }
    })
}

if (requireNamespace("RSQLite", quietly = TRUE)) {
    test_that("read.rsk() values, with a local file", {
        # This will not work with arbitrary rsk files, since e.g. checks are
        # made against the device serial number and the software version
        # numbers.
        f <- "local_data/060130_20150904_1159.rsk"
        if (file.exists(f)) {
            rsk <- read.oce(f)
            expect_equal(rsk[["model"]], "RBRconcerto")
            expect_equal(rsk[["pressureType"]], "absolute")
            expect_equal(rsk[["pressureAtmospheric"]], 10.1325)
            expect_equal(rsk[["serialNumber"]], 60130)
            expect_equal(rsk[["sampleInterval"]], 0.167)
            expect_equal(rsk[["rskVersion"]], c(1, 9, 0))
            expect_equal(rsk[["ruskinVersion"]], c(1, 10, 0))
            expect_equal(sort(names(rsk[["data"]])), c("conductivity", "pressure", "temperature", "time", "tstamp"))
            # Note: the internal representation, checked below, is a bit
            # counter-intuitive for a time in milliseconds, but this is just a
            # reflection of how the storage is done.  Another way to see the
            # data is as follows, and these numbers make sense. Please see
            # https://github.com/dankelley/oce/issues/2062 for more discussion.
            #     > rsk@data$tstamp[1:3]
            #     integer64
            #     [1] 1441380732000 1441380732167 1441380732333
            expect_equal(
                rsk@data$tstamp[1:3],
                structure(c(7.12136702258709e-312, 7.12136702341218e-312, 7.12136702423233e-312), class = "integer64")
            )
        }
    })
}
