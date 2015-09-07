## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

## FIXME: commented-out data() lines indicate things to be done; keep
## to alphabetical order, please.

##data("adp")
##data("adv")

test_that("cm", {
          data("cm")
          S <- cm[["salinity"]]
          S1 <- swSCTp(cm)
          S2 <- swSCTp(cm[['conductivity']],cm[['temperature']], cm[['pressure']],
                       conductivityUnit=cm[['conductivityUnit']])
          expect_equal(max(abs(S1-S2)), 0)
          expect_less_than(mean(abs(S-S1)), 0.001)
          expect_less_than(median(abs(S-S1)), 0.0011)
          ## I am not sure why these differ by 0.003PSU. The data file lists S
          ## to 0.001, T to 0.001, and depth to 0.001. I'm not terribly worried
          ## about the 0.003 disagreement, however, because the actual values
          ## appear to be wrong by a lot more than that ... I don't believe the
          ## deep salinities in the St Lawrence Estuary are in excess of 40.
          expect_less_than(max(abs(S-S1)), 0.003)
})

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
test_that("topoWorld", {
          data("topoWorld")
          expect_equal(range(topoWorld[["longitude"]]), c(-179.5, 180))
          expect_equal(range(topoWorld[["latitude"]]), c(-89.5, 90))
})
##data("wind")

