## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

## FIXME: commented-out data() lines indicate things to be done.

## Part 1: various tests, some of them ad-hock

##data("adp")
##data("adv")
##data("argo")

context("datasets")

test_that("cm", {
          data("cm")
          S <- cm[["salinity"]]
          S1 <- swSCTp(cm)
          expect_equal(S, S1, tolerance=0.001)
          S2a <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']], conductivityUnit=cm[['conductivity unit']])
          expect_equal(S1, S2a)
          S2b <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']], conductivityUnit=cm[['conductivityUnit']])
          expect_equal(S1, S2b)
          S2c <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']], conductivityUnit=as.character(cm[['conductivityUnit']]$unit))
          expect_equal(S1, S2c)
})

##data("coastlineWorld")

test_that("ctd", {
          data("ctd")
          expect_equal(ctd[["latitude"]],   44.6842666666667)
          expect_equal(ctd[["longitude"]], -63.6438833333333)
          ## next two lines test issues 1460 and 1547
          expect_equal(ctd[["time"]][1], as.POSIXct("2003-10-15 15:40:47", tz="UTC"))
          expect_equal(length(ctd[["time"]]), length(ctd[["pressure"]]))
          ## units are checked in test_accessors.R
          expect_equal(ctd[["pressureType"]], "sea")
})

##data("ctdRaw")
##data("echosounder")
##data("lisst")
##data("lobo")
##data("rsk")
##data("met")

test_that("sealevel", {
          data("sealevel")
          expect_equal(sealevel[["latitude"]],   44.666667)
          expect_equal(sealevel[["longitude"]], -63.583333)
          expect_equal(sealevel[["stationNumber"]], 490)
          expect_equal(sealevel[["GMTOffset"]], 0)
})

##data("sealevelTuktoyaktuk")

test_that("sealevel", {
          data("section")
          expect_equal(section[["sectionId"]], "a03")
          expect_equal(length(section@data$station), 124)
          expect_equal(section@data$station[[1]]@metadata$station, "3")
          expect_equal(section@data$station[[1]][["station"]], "3")
          expect_equal(section@data$station[[1]][["latitude"]], 36.8758)
          expect_equal(section@data$station[[1]][["longitude"]], -8.5263)
})

##data("tidedata")
test_that("topoWorld", {
          data("topoWorld")
          expect_equal(range(topoWorld[["longitude"]]), 179.75*c(-1, 1))
          expect_equal(range(topoWorld[["latitude"]]), 89.75*c(-1, 1))
})
##data("wind")


## Part 2: check units

## below constructed from the files in oce/data
test_that("units", {
          names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "ocecolors", "ctd",
                     "ctdRaw", "echosounder", "landsat", "lisst", "lobo", "met", "rsk",
                     "sealevel", "sealevelTuktoyaktuk", "section", "tidedata",
                     "topoWorld", "wind")
          for (name in names) {
              data(list=name)
              if (name == "section") {
                  x <- get(name)
                  x <- x[["station", 1]]
              } else {
                  x <- get(name)
              }
              if ("metadata" %in% slotNames(x) && "units" %in% names(x@metadata)) {
                  units <- x@metadata$units
                  if (length(units) > 0) {
                      unitsNames <- names(units)
                      for (i in seq_along(units)) {
                          this <- units[[i]]
                          expect_equal(2, length(this))
                          expect_equal(2, sum(c("unit", "scale") %in% names(this)))
                          expect_true(is.expression(this$unit))
                          expect_true(is.character(this$scale))
                      }
                  }
              }
          }
})
