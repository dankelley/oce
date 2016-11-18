library(oce)
context("echosounder")
test_that("biosonics", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              e <- read.oce("local_data/echosounder.dt4")
              expect_equal("single-beam", e[["beamType"]])
              expect_equal(c(308, 3399), dim(e[["a"]]))
              summary(e)
              ## plot(e)
          }
})

