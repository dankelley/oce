library(oce)
context("echosounder")
test_that("biosonics", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              e <- read.oce("local_data/echosounder.dt4")
              expect_equal("single-beam", e[["beamType"]])
              expect_equal(c(308, 3399), dim(e[["a"]]))
              summary(e)
              expect_warning(plot(e),
                             "auto-decimating second index of large image by 8; use decimate=FALSE to prevent this")
          }
})

