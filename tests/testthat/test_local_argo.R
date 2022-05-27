# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#ncvar_get() failed for "HISTORY_STOP_PRES", so it isn't stored in metadata
#ncvar_get() failed for "HISTORY_PREVIOUS_VALUE", so it isn't stored in metadata


library(oce)
test_that("the data(argo) dataset", {
    data(argo)
    expect_equal(argo[["id"]][1], "6900388")
    expect_equal(dim(argo[["pressure"]]), c(56,223))
    expect_equal(sort(tolower(names(argo[["data"]]))),
        tolower(c("latitude", "longitude", "pressure",
                "pressureAdjusted", "pressureAdjustedError",
                "salinity", "salinityAdjusted",
                "salinityAdjustedError", "temperature",
                "temperatureAdjusted", "temperatureAdjustedError",
                "time")))
})

if (1 == length(list.files("6900388_prof.nc", path="local_data")) && requireNamespace("ncdf4", quietly=TRUE)) {
    test_that("the data from which data(argo) was constructed", {
        a <- read.oce("local_data/6900388_prof.nc")
        expect_equal(a[["id"]][1], "6900388")
        expect_equal(dim(a[["pressure"]]), c(56,223))
        expect_equal(sort(tolower(names(a[["data"]]))),
            tolower(c("latitude", "longitude", "pressure",
                    "pressureAdjusted", "pressureAdjustedError",
                    "salinity", "salinityAdjusted",
                    "salinityAdjustedError", "temperature",
                    "temperatureAdjusted", "temperatureAdjustedError",
                    "time")))
})}

if (1 == length(list.files("BR5904179_001.nc", path="local_data")) && requireNamespace("ncdf4", quietly=TRUE)) {
    test_that("a bioargo dataset", {
        if (1 == length(list.files(path=".", pattern="local_data"))) {
            expect_silent(a <- read.oce("local_data/BR5904179_001.nc"))
            expect_equal(a[["id"]][1], "5904179")
            expect_equal(dim(a[["pressure"]]), c(499,2))
            expect_equal(sort(tolower(names(a@data))),
                tolower(c("BBP700", "BBP700Adjusted", "BBP700AdjustedError",
                        "betaBackscattering700", "bphaseOxygen",
                        "chlorophyllA", "chlorophyllAAdjusted",
                        "chlorophyllAAdjustedError",
                        "fluorescenceChlorophyllA", "latitude",
                        "longitude", "nitrate", "nitrateAdjusted",
                        "nitrateAdjustedError", "oxygen", "oxygenAdjusted",
                        "oxygenAdjustedError", "pressure",
                        "temperatureOxygen", "time",
                        "UVIntensityDarkNitrate", "UVIntensityNitrate")))
        }
})}

