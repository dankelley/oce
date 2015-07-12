## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

## FIXME: commented-out data() lines indicate things to be done; keep
## to alphabetical order, please.

##data("adp")
##data("adv")
##data("cm")
##data("coastlineWorld")

test_that("ctd", {
          data("ctd")
          expect_equal(ctd[["latitude"]],   44.6842666666667)
          expect_equal(ctd[["longitude"]], -63.6438833333333)
          expect_equal(ctd[["startTime"]], as.POSIXct("2003-10-15 11:38:38", tz="UTC"))
})

##data("ctdRaw")
##data("drifter")
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
##data("topoWorld")
##data("wind")

