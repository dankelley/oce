library(testthat)

test_that("download.met works for hourly data", {
    f <- download.met(8204700, 2007, 6, deltat = "hour", type = "csv",
                      destdir = tempdir())
    expect_true(file.exists(f))

    # Should not be HTML
    first_line <- readLines(f, n = 1, warn = FALSE)
    expect_false(grepl("<!DOCTYPE html", first_line))

    # Should be readable as met object
    m <- read.met(f)
    expect_s4_class(m, "met")
    expect_true("temperature" %in% names(m@data))
})

test_that("download.met works for monthly data", {
    f <- download.met(8204700, 2007, 6, deltat = "month", type = "xml",
                      destdir = tempdir())
    expect_true(file.exists(f))

    first_line <- readLines(f, n = 1, warn = FALSE)
    expect_false(grepl("<!DOCTYPE html", first_line))

    m <- read.met(f)
    expect_s4_class(m, "met")
})

test_that("download.met works with XML format", {
    f <- download.met(8204700, 2007, 6, deltat = "hour", type = "xml",
                      destdir = tempdir())
    m <- read.met(f)
    expect_s4_class(m, "met")
})
