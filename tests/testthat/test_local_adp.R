library(oce)
context("read adp data")
test_that("Teledyn/RDI read (integer from,to) and check", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adp_rdi",
                               from=1, to=10, latitude=47.88126, longitude=-69.73433)
              xyz <- beamToXyzAdp(beam)
              enu <- xyzToEnuAdp(xyz, declination=-18.1)
              expect_equal(c(10, 84, 4), dim(beam[["v"]]))
              expect_equal(c(10, 84, 4), dim(xyz[["v"]]))
              expect_equal(c(10, 84, 4), dim(enu[["v"]]))
              expect_equal(beam[["latitude"]], 47.88126)
              expect_equal(xyz[["latitude"]], 47.88126)
              expect_equal(enu[["latitude"]], 47.88126)
              expect_equal(beam[["longitude"]], -69.73433)
              expect_equal(xyz[["longitude"]], -69.73433)
              expect_equal(enu[["longitude"]], -69.73433)
              ## FIXME: add more tests on the data
          }
})

test_that("Teledyn/RDI read (POSIXct from,to)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adp_rdi",
                               from=as.POSIXct("2008-06-25 10:01:00",tz="UTC"),
                               to=as.POSIXct("2008-06-25 10:03:00",tz="UTC"))
              expect_true(is.na(beam[["latitude"]]))
              expect_true(is.na(beam[["longitude"]]))
              expect_true(dim(beam[["v"]]), c(14,84,4))
          }
})

test_that("Teledyn/RDI binmap", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adp_rdi",
                               from=1, to=10, latitude=47.88126, longitude=-69.73433)
              beam2 <- binmapAdp(beam)
              ## FIXME: add tests on the data
          }
})

test_that("Nortek aquadopp read and check", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adp_nortek_aquadopp",
                               from=1, to=10, latitude=47.87943, longitude=-69.72533)
              xyz <- beamToXyzAdp(beam)
              enu <- xyzToEnuAdp(xyz, declination=-18.1)
              expect_equal(c(10, 25, 3), dim(beam[["v"]]))
              expect_equal(c(10, 25, 3), dim(xyz[["v"]]))
              expect_equal(c(10, 25, 3), dim(enu[["v"]]))
              expect_equal(beam[["latitude"]], 47.87943)
              expect_equal(xyz[["latitude"]], 47.87943)
              expect_equal(enu[["latitude"]], 47.87943)
              expect_equal(beam[["longitude"]], -69.72533)
              expect_equal(xyz[["longitude"]], -69.72533)
              expect_equal(enu[["longitude"]], -69.72533)
               ## FIXME: add more tests on the data
          }
})

test_that("Sortek (PCADP)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              beam <- read.oce("local_data/adp_sontek",
                               from=1, to=10, latitude=48.87961, longitude=-69.72706)
              expect_equal(48.87961, beam[["latitude"]])
              expect_equal(-69.72706, beam[["longitude"]])
          }
})

