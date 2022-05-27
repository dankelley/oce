library(oce)
test_that("SOI index", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              SOI <- read.index("local_data/SOI.signal.ascii")
              expect_equal(names(SOI), c("t", "index"))
              expect_equal(SOI$t[1:3], as.POSIXct(c("1866-01-15", "1866-02-15", "1866-03-15"), tz="UTC"))
              expect_equal(SOI$index[1:3], c(-1.2, -0.3, -1.0))
          }
})

