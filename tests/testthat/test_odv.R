# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

# This ODV file was provided in an issue report; see
# https://github.com/dankelley/oce/issues/1696
test_that("local sample01.txt (from a user)", {
    if (1 == length(list.files(path=".", pattern="local_data"))) {
        expect_silent(d <- read.ctd("local_data/odv/sample01.txt", type="ODV"))
        expect_output(summary(d), "^CTD Summary")
    }
})

# https://www.seadatanet.org/Standards/Data-Transport-Formats
test_that("local sample02.txt (basic CTD, from seadatanet.org)", {
    if (1 == length(list.files(path=".", pattern="local_data"))) {
        expect_silent(d <- read.ctd("local_data/odv/sample02.txt", type="ODV"))
        expect_output(summary(d), "^CTD Summary")
    }
})

test_that("local sample03.txt (CTD with instruments, from seadatanet.org)", {
    if (1 == length(list.files(path=".", pattern="local_data"))) {
        expect_silent(d <- read.ctd("local_data/odv/sample03.txt", type="ODV"))
        expect_output(summary(d), "^CTD Summary")
    }
})

test_that("local sample04.txt (CTD with instruments, from seadatanet.org)", {
    if (1 == length(list.files(path=".", pattern="local_data"))) {
        expect_silent(d <- read.ctd("local_data/odv/sample04.txt", type="ODV"))
        expect_output(summary(d), "^CTD Summary")
    }
})


# below is test code for testing updates to the column renaming.
if (FALSE) {
    source("~/git/oce/R/processingLog.R")
    source("~/git/oce/R/ctd.R")
    source("~/git/oce/R/ctd.odv.R")
    sink("test_odv.out")
    for (file in list.files(path="local_data/odv", pattern="*.txt", full.names=TRUE)) {
        print(file)
        d <- read.ctd(file, type="ODV")
        summary(d)
    }
    sink()
}
