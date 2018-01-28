library(oce)
context("Bremen data format")

test_that("lowered adcp", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              b <- read.bremen("local_data/msm27_003.ladcp")
              expect_equal(sort(names(b@data)), c("pressure", "u", "uz", "v", "vz"))
              expect_equal(b[["ship"]], "Maria S. Merian")
              expect_equal(b[["station"]], "142")
              expect_equal(b[["cruise"]], "27")
              expect_equal(b[["longitude"]], -46.8539833333333)
              expect_equal(b[["latitude"]], 47.1067833333333)
              expect_equal(b[["time"]], as.POSIXct("2013-04-20 14:20:00", tz="UTC"))
              ##> summary(b)
              ##> plot(b)
          }
})


