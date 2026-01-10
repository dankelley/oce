# This is a 100K snippet of a 10M RDI adp dataset in workhorse II format, from
# Clark Richards in January 2026. The file has the usual things (not checked
# here) but what's new for Workorse II format is ISM, for which code was
# written 2026-01-10 for issue https://github.com/dankelley/oce/issues/2353.

library(oce)
library(testthat)
file <- "local_data/rdi_workhorse/whII_snippet.000"
if (file.exists(file)) {
    expect_output(adp <- read.oce(file), "EOF at cindex")
    expect_true(all(adp[["ISMvalid"]] == 1))
    expect_equal(
        adp[["ISMacc"]][1:5, ],
        structure(c(
            8, 11, 14, 12, 14, -7, -7, -4, -4, -11, 1004, 1002,
            1005, 1000, 1004
        ), dim = c(5L, 3L))
    )
    expect_equal(
        adp[["ISMmag"]][1:5, ],
        structure(c(
            180, 180, 181, 180, 181, 496, 495, 493, 500, 492,
            533, 535, 535, 536, 536
        ), dim = c(5L, 3L))
    )
}
