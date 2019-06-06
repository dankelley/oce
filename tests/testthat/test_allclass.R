## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("allclass")

test_that("general", {
          o <- new("oce")
          expect_equal(c("data", "metadata", "processingLog"), sort(slotNames(o)))
          expect_equal(2, length(o[['processingLog']]))
          expect_null(o[['no_such_thing']])
          expect_output(show(o), "oce object has nothing in its data slot.")
          expect_warning(plot(o), "no data to plot")
          ## subsets of base oce object
          oo <- new("oce")
          oo2 <- oceSetData(oo, "a", 1:10)
          subset(oo2, a < 5)
          ## insert data
          expect_null(o[['noSuchThing']])
          o[['noSuchThing']] <- 0
          expect_equal(o[['noSuchThing']], 0)
})

test_that("oceDeleteData clears flags and units", {
          data(section)
          ctd <- section[["station", 1]]
          withoutSalinity <- oceDeleteData(ctd, "salinity")
          expect_equal(names(ctd@metadata$flags),
                       c("salinity", "salinityBottle", "oxygen", "silicate", "nitrite", "NO2+NO3", "phosphate"))
          expect_equal(names(ctd[["flags"]]),
                       c("salinity", "salinityBottle", "oxygen", "silicate", "nitrite", "NO2+NO3", "phosphate"))

          expect_equal(names(ctd[["units"]]),
                       c("pressure", "temperature", "salinity", "salinityFlag",
                         "salinityBottle", "salinityBottleFlag", "oxygen",
                         "oxygenFlag", "silicate", "silicateFlag", "nitrite",
                         "nitriteFlag", "NO2+NO3", "NO2+NO3Flag", "phosphate",
                         "phosphateFlag"))
          expect_equal(names(withoutSalinity[["units"]]),
                       c("pressure", "temperature", "salinityFlag",
                         "salinityBottle", "salinityBottleFlag", "oxygen",
                         "oxygenFlag", "silicate", "silicateFlag", "nitrite",
                         "nitriteFlag", "NO2+NO3", "NO2+NO3Flag", "phosphate",
                         "phosphateFlag"))
})

