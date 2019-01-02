## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

###################################################################################
## IMPORTANT: Update this and ?oce-deprecated, whenever function status changes. ##
###################################################################################

## NOTE: '+' indicates items already handled; line numbers may be incorrect.
## $ git grep -n '\.Deprecated'
## $ git grep -n '\.Defunct' 
## +ctd.R:1453:    .Defunct("oceSetData",
## +ctd.R:2527:    .Defunct("oceSetMetadata",
## +map.R:1848:    .Defunct("mapGrid",
## +map.R:2141:    .Defunct("mapGrid",
## +misc.R:33:    .Defunct("oceSetData",
## +misc.R:1248:    .Defunct("findInterval",
## +misc.R:3627:        .Defunct("rawToBits",
## +oce.R:1265:    .Defunct("lubridate::parse_date_time",


library(oce)

context("deprecate and defunct")

test_that("defunct", {
          data(ctd)
          expect_error(addColumn(ctd, ctd[["pressure"]], "pressure"), # 0.9.24
                       "addColumn\\(\\) is disallowed and will be removed soon. Use oceSetData\\(\\) instead. See \\?'oce-defunct'.")
          expect_error(byteToBinary(as.raw(0x01), "little"), # 0.9.24
                       "byteToBinary\\(.,'little'\\) is disallowed and will be removed soon. See \\?'oce-defunct'.")
          expect_error(ctdAddColumn(ctd, ctd[["pressure"]], "pressure", "Pressure [dbar]"), # 0.9.24
                       "ctdAddColumn\\(\\) is disallowed and will be removed soon. Use oceSetData\\(\\) instead. See \\?'oce-defunct'.")
          expect_error(ctdUpdateHeader(ctd), # 0.9.24
                       "ctdUpdateHeader\\(\\) is disallowed and will be removed soon. See \\?'oce-defunct'.")
          expect_error(findInOrdered(1, 2), # 0.9.24
                       "findInOrdered\\(\\) is disallowed and will be removed soon. Use findInterval\\(\\) instead. See \\?'oce-defunct'.")
          expect_error(oce.as.POSIXlt("1999-01-01"), # 0.9.24
                       "oce.as.POSIXlt\\(\\) will be removed soon. Use lubridate::parse_date_time\\(\\) instead. See \\?'oce-defunct'.")
          if (interactive()) { # lets me run the test manually, without devtools::test() giving a warning.
            a <- as.coastline(1:3, 1:3)
            mapPlot(a)
            expect_error(mapZones(),
                         "mapZones\\(\\) will be removed soon. Use mapGrid\\(\\) instead. See \\?'oce-defunct'")
            expect_error(mapMeridians(),
                         "mapMeridians\\(\\) will be removed soon. Use mapGrid\\(\\) instead. See \\?'oce-defunct'")
          }
          expect_error(oce.magic(),
                       "could not find function \"oce.magic\"")
})


