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

test_that("rename() with skeleton objects (with user's list dictionary)", {
    if (1L == length(list.files(path = ".", pattern = "local_data"))) {
        colnames <- strsplit(readLines("local_data/rename.csv", encoding = "latin1", n = 1), ",")[[1]]
        d <- read.csv("local_data/rename.csv", encoding = "latin1", skip = 1)
        names(d) <- colnames
        o <- new("oce")
        for (name in names(d)) {
            o@data[name] <- d[name]
        }
        oo <- rename(o, "sbe")
        expect_equal(
            names(oo[["data"]]),
            c(
                "scan", "pressure", "depth", "temperature", "temperature2",
                "conductivity", "conductivity2", "oxygenRaw", "beamTransmission",
                "v1", "fluorescence", "v0", "fluorescence2", "v4", "upoly", "PAR",
                "spar", "altimeter", "oxygen", "salinity", "salinity2", "theta",
                "sigmaTheta", "soundSpeed", "nbin", "flag"
            )
        )
    }
})


test_that("rename() and built-in renaming", {
    f <- system.file("extdata", "ctd.cnv.gz", package = "oce")
    expect_warning(
        expect_warning(
            d1 <- read.ctd(f),
            "suspicious startTime 1903-10-15 11:38:38 changed to 2003-10-15 11:38:38"
        ),
        "IPTS-68"
    )
    expect_warning(
        tmp <- read.ctd(f, rename = FALSE),
        "suspicious startTime 1903-10-15 11:38:38 changed to 2003-10-15 11:38:38"
    )
    d2 <- tmp |> rename("sbe")
    expect_equal(d1[["data"]], d2[["data"]])
    expect_equal(d1[["metadata"]], d2[["metadata"]])
})
