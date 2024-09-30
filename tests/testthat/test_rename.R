library(oce)

test_that("rename() with skeleton objects (with built-in dictionary)", {
    data(ctd)
    x <- new("ctd")
    x@data$PSAL <- ctd[["salinity"]]
    x@data$t090 <- ctd[["temperature"]]
    x@data$prM <- ctd[["pressure"]]
    x@data$depS <- ctd[["depth"]]
    # Create some flags to see if they get renamed properly
    x@metadata$flags <- list()
    x@metadata$flags$PSAL <- rep(1, length(x@data$PSAL))
    y <- rename(x, "sbe")
    expect_equal(x[["PSAL"]], y[["salinity"]])
    expect_equal(x[["t090"]], y[["temperature"]])
    expect_equal(x[["prM"]], y[["pressure"]])
    expect_equal(x[["depS"]], y[["depS"]])
    expect_equal(x[["flags"]]$PSAL, y[["salinityFlag"]])
    expect_equal(x[["flags"]]$PSAL, y[["flags"]]$salinity)
})

test_that("rename() with skeleton objects (with user's file dictionary)", {
    data(ctd)
    dfile <- tempfile(fileext = ".csv") # removed at end of test
    d <- "SALINITY,salinity,,
TEMPERATURE,temperature,degree*C,ITS-90"
    write(d, dfile)
    x <- new("ctd")
    x@data$SALINITY <- ctd[["salinity"]]
    x@data$TEMPERATURE <- ctd[["temperature"]]
    # Create some flags to see if they get renamed properly
    x@metadata$flags <- list()
    x@metadata$flags$SALINITY <- rep(1, length(x@data$SALINITY))
    y <- rename(x, dfile)
    expect_equal(x[["SALINITY"]], y[["salinity"]])
    expect_equal(x[["TEMPERATURE"]], y[["temperature"]])
    expect_equal(x[["flags"]]$SALINITY, y[["salinityFlag"]])
    expect_equal(x[["flags"]]$SALINITY, y[["flags"]]$salinity)
    unlink(dfile)
})

test_that("rename() with skeleton objects (with user's list dictionary)", {
    data(ctd)
    d <- "SALINITY,salinity,,
TEMPERATURE,temperature,degree*C,ITS-90"
    dictionary <- read.csv(text = d, header = FALSE)
    x <- new("ctd")
    x@data$SALINITY <- ctd[["salinity"]]
    x@data$TEMPERATURE <- ctd[["temperature"]]
    # Create some flags to see if they get renamed properly
    x@metadata$flags <- list()
    x@metadata$flags$SALINITY <- rep(1, length(x@data$SALINITY))
    y <- rename(x, dictionary)
    expect_equal(x[["SALINITY"]], y[["salinity"]])
    expect_equal(x[["TEMPERATURE"]], y[["temperature"]])
    expect_equal(x[["flags"]]$SALINITY, y[["salinityFlag"]])
    expect_equal(x[["flags"]]$SALINITY, y[["flags"]]$salinity)
})
