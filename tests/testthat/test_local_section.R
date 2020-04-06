library(oce)
context("local section")
test_that("BOTTLE type", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              s <- read.oce("local_data/77DN20020420_hy1.csv")
              expect_true(inherits(s, "section"))
              expect_equal(92, length(s[["station"]]))
              expect_equal(s[["station",1]][["latitude"]], 77.1695)
              expect_equal(s[["station",1]][["longitude"]], 19.3618)
              expect_equal(s[["station",90]][["station"]], "94")
          }
})


