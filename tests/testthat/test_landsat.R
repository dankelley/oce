library(oce)
context("landsat")
test_that("landsatTrim", {
          data(landsat)
          lt <- landsatTrim(landsat,list(longitude=-64,latitude=44),list(longitude=-63,latitude=45))
          show(lt)
          expect_equal(lt[["lllat"]], 44)
          expect_equal(lt[["urlat"]], 45)
          expect_equal(lt[["lllon"]], -64)
          expect_equal(lt[["urlon"]], -63)
          expect_equal(c(27, 38), dim(lt[["tirs1"]]))
})


