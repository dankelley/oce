## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(argo)

context("Argo")

test_that("subset.argo(argo, \"adjusted\") correctly alters metadata and data", {
          a <- subset(argo, "adjusted")
          expect_equal(a@metadata$flags$pressureQc, argo@metadata$flags$pressureAdjustedQc) 
          expect_equal(a@metadata$flags$temperatureQc, argo@metadata$flags$temperatureAdjustedQc) 
          expect_equal(a@metadata$flags$salinityQc, argo@metadata$flags$salinityAdjustedQc) 
          expect_equal(a@metadata$flags$pressure, argo@metadata$flags$pressureAdjusted) 
          expect_equal(a@metadata$flags$salinity, argo@metadata$flags$salinityAdjusted) 
          expect_equal(a@metadata$flags$temperature, argo@metadata$flags$temperatureAdjusted) 
})
 
