library(oce)
context("read adv data")
test_that("nortek_vector", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adv_nortek_vector", from=1, to=10,
                               latitude=47.87943, longitude=-69.72533)
              xyz <- beamToXyzAdv(beam)
              adp <- xyzToEnuAdv(xyz)
              ## FIXME: add some tests on the data here
          }
})

