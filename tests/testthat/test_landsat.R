library(oce)
context("landsat")

f <- "/data/archive/landsat/LC80080292014065LGN00"
if (file.exists(f)) {
    test_that("read landsat (private file)", {
              expect_warning(l <- read.landsat(f, band="tirs1"))
              expect_equal(dim(l@data$tirs1$msb), c(7861, 7991))
              expect_equal(dim(l@data$tirs1$lsb), c(7861, 7991))
              expect_equal(l@data$tirs1$lsb[2000, 2000:2005],
                           as.raw(c(0xb1, 0x94, 0x77, 0x5a, 0x47, 0x41)))
              expect_equal(l@data$tirs1$msb[2000:2005, 2000],
                           as.raw(c(0x3c, 0x3c, 0x3c, 0x3c, 0x3b, 0x3b)))
})
}


test_that("landsatTrim", {
          data(landsat)
          lt <- landsatTrim(landsat,list(longitude=-64,latitude=44),list(longitude=-63,latitude=45))
          expect_equal(dim(lt[["aerosol"]]), c(27,38))
          expect_equal(dim(lt[["blue"]]), c(27,38))
          expect_equal(dim(lt[["green"]]), c(27,38))
          expect_equal(dim(lt[["red"]]), c(27,38))
          expect_equal(dim(lt[["nir"]]), c(27,38))
          expect_equal(dim(lt[["swir1"]]), c(27,38))
          expect_equal(dim(lt[["swir2"]]), c(27,38))
          expect_equal(dim(lt[["panchromatic"]]), c(55,74))
          expect_equal(dim(lt[["cirrus"]]), c(27,38))
          expect_equal(dim(lt[["tirs1"]]), c(27,38))
          expect_equal(dim(lt[["tirs2"]]), c(27,38))
          expect_equal(lt[["lllat"]], 44)
          expect_equal(lt[["urlat"]], 45)
          expect_equal(lt[["lllon"]], -64)
          expect_equal(lt[["urlon"]], -63)
          expect_equal(c(27, 38), dim(lt[["tirs1"]]))
})


