library(oce)
file <- "local_data/ad2cp/S102791A003_Barrow_2022_0001_sub.ad2cp"
if (file.exists(file)) {
    skip_on_cran()
    test_that("'subset' works (issue 2320)", {
        expect_message(expect_warning(
            bar <- read.adp.ad2cp(file, dataType = "burstAltimeterRaw"),
            "early EOF in chunk"
        ), "setting plan=0")
        start <- mean(bar[["time"]])
        ntime <- sum(bar[["time"]] > mean(bar[["time"]]))
        sub <- subset(bar, time >= start)
        expect_equal(ntime, length(sub[["altimeterRawTime"]]))
        expect_equal(dim(sub[["altimeterRawSamples"]]), c(length(sub[["altimeterRawTime"]]), sub[["altimeterRawNumberOfSamples"]]))
        expect_equal(length(bar[["altimeterRawDistance"]]), length(sub[["altimeterRawDistance"]]))
        expect_equal(1L, length(sub[["altimeterRawNumberOfSamples"]]))
        expect_equal(1L, length(sub[["altimeterRawBlankingDistance"]]))
        expect_equal(1L, length(sub[["altimeterRawSampleDistance"]]))
    })
}
