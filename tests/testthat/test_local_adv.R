library(oce)
context("read adv data")
test_that("nortek vector", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adv_nortek_vector", from=1, to=10,
                               latitude=47.87943, longitude=-69.72533)
              xyz <- beamToXyzAdv(beam)
              enu <- xyzToEnuAdv(xyz)
              ## FIXME: add some tests on the data here
          }
})

test_that("sontek", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              xyz <- read.adv.sontek.adr("local_data/adv_sontek", from=1, to=20,
                                         latitude=47.87943, longitude=-69.72533)
              expect_equal(c(19, 3), dim(xyz[["v"]]))
              expect_equal(-69.72533, xyz[["longitude"]])
              expect_equal(47.87943, xyz[["latitude"]])
              expect_equal("xyz", xyz[["originalCoordinate"]])
              ## FIXME: add some tests on the data here
          }
})

