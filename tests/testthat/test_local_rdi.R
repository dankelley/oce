library(oce)
context("read adp data")
test_that("Teledyn/RDI", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/rdi", from=1, to=10, latitude=47.88126, longitude=-69.73433)
              xyz <- beamToXyzAdp(beam)
              adp <- xyzToEnuAdp(xyz, declination=-18.1)
              ## FIXME: add some tests on the data here
          }
})

