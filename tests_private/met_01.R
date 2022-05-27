## As of 20180405, hcd_hourly() cannot read the env-can data. I sent
## a patch.
if (FALSE) {
    library(oce)
    library(testthat)

    if (requireNamespace("canadaHCD", quietly=TRUE)) {
        test_that("as.met() works", {
            ## devtools::install_github("gavinsimpson/canadaHCD")
            a <- canadaHCD::hcd_hourly(6358, 2003, 9)
            MET <- as.met(a)
            MET[["time"]] <- MET[["time"]] + 4 * 3600 # get into UTC
            data(met)
            testthat::expect_equal(MET[["time"]], met[["time"]])
            testthat::expect_equal(MET[["temperature"]], met[["temperature"]])
            testthat::expect_equal(MET[["pressure"]], met[["pressure"]])
            testthat::expect_equal(MET[["speed"]], met[["speed"]])
            testthat::expect_equal(MET[["direction"]], met[["direction"]])
            testthat::expect_equal(MET[["u"]], met[["u"]])
            testthat::expect_equal(MET[["v"]], met[["v"]])
})
    }

    test_that("download/read/summary works with hourly data (csv)", {
        ## remove cached version so we are testing the server as well as the code
        f <- download.met(id=6358, year=2003, month=9, destdir=".", type="csv", force=TRUE, quiet=TRUE)
        m <- read.met(f)
        expect_output(summary(m), "Met Summary")
        if (!interactive()) png("met_01_01.png")
        plot(m)
        if (!interactive()) dev.off()
        file.remove(f)
})


    test_that("download/read/summary works with hourly data (xml)", {
        ## remove cached version so we are testing the server as well as the code
        f <- download.met(id=6358, year=2003, month=9, destdir=".", type="xml", force=TRUE, quiet=TRUE)
        m <- read.met(f)
        expect_output(summary(m), "Met Summary")
        if (!interactive()) png("met_01_02.png")
        plot(m)
        if (!interactive()) dev.off()
        file.remove(f)
})

    test_that("download/read/summary works with monthly data (csv)", {
        ## remove cached version so we are testing the server as well as the code
        f <- download.met(id=6358, deltat="month", destdir=".", type="csv", force=TRUE, quiet=TRUE)
        m <- read.met(f)
        expect_output(summary(m), "Met Summary")
        if (!interactive()) png("met_01_03.png")
        plot(m)
        if (!interactive()) dev.off()
        file.remove(f)
})

    test_that("download/read/summary works with monthly data (xml)", {
        ## remove cached version so we are testing the server as well as the code
        f <- download.met(id=6358, deltat="month", destdir=".", type="xml", force=TRUE, quiet=TRUE)
        m <- read.met(f)
        expect_output(summary(m), "Met Summary")
        if (!interactive()) png("met_01_04.png")
        plot(m)
        if (!interactive()) dev.off()
        file.remove(f)
})

    files <- list.files(path=".", pattern="eng-hourly.*csv", full.names=TRUE)
    if (length(files)) {
        test_that("read.met() works on some files downloaded in 2009", {
            for (file in files) {
                d <- expect_silent(read.met(file))
            }
})
    }
}
