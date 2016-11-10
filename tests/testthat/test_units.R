## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("Units")

test_that("as.unit", {
          expect_equal(as.unit("DBAR"), list(unit=expression(dbar), scale=""))
          expect_equal(as.unit("IPTS-68"), list(unit=expression(degree*C), scale="IPTS-68"))
          expect_equal(as.unit("ITS-90"), list(unit=expression(degree*C), scale="ITS-90"))
          expect_equal(as.unit("PSS-78"), list(unit=expression(), scale="PSS-78"))
          expect_equal(as.unit("UMOL/KG"), list(unit=expression(mu*mol/kg), scale=""))
          expect_equal(as.unit(), list(unit=expression(), scale=""))
})          

