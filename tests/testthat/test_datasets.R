## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

## FIXME: commented-out data() lines indicate things to be done.

## Part 1: various tests, some of them ad-hock

##data("adp")
##data("adv")
##data("argo")

test_that("cm", {
          data("cm")
          S <- cm[["salinity"]]
          S1 <- swSCTp(cm)
          expect_lt(mean(abs(S-S1)), 0.001)
          expect_lt(median(abs(S-S1)), 0.0011)
          S2a <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']], conductivityUnit=cm[['conductivity unit']])
          expect_equal(S1, S2a)
          S2b <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']], conductivityUnit=cm[['conductivityUnit']])
          expect_equal(S1, S2b)
          S2c <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']], conductivityUnit=as.character(cm[['conductivityUnit']]$unit))
          expect_equal(S1, S2c)
          ## I am not sure why these differ by 0.003PSU. The data file lists S
          ## to 0.001, T to 0.001, and depth to 0.001. I'm not terribly worried
          ## about the 0.003 disagreement, however, because the actual values
          ## appear to be wrong by a lot more than that ... I don't believe the
          ## deep salinities in the St Lawrence Estuary are in excess of 40.
          expect_lt(max(abs(S-S1)), 0.003)
})

##data("coastlineWorld")

test_that("ctd", {
          data("ctd")
          expect_equal(ctd[["latitude"]],   44.6842666666667)
          expect_equal(ctd[["longitude"]], -63.6438833333333)
          expect_equal(ctd[["startTime"]], as.POSIXct("2003-10-15 11:38:38", tz="UTC"))
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
          stopifnot(all.equal(section[["sectionId"]], "a03"))
          expect_equal(length(section@data$station), 124)
          expect_equal(section@data$station[[1]]@metadata$station, "3")
          expect_equal(section@data$station[[1]][["station"]], "3")
          expect_equal(section@data$station[[1]][["latitude"]], 36.8758)
          expect_equal(section@data$station[[1]][["longitude"]], -8.5263)
})

##data("tidedata")
test_that("topoWorld", {
          data("topoWorld")
          expect_equal(range(topoWorld[["longitude"]]), c(-179.5, 180))
          expect_equal(range(topoWorld[["latitude"]]), c(-89.5, 90))
})
##data("wind")


## Part 2: check units

## below constructed from the files in oce/data
test_that("units", {
          names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "colors", "ctd",
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
