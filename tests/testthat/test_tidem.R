## vim:textwidth=120:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(sealevel)

context("tidem")

## Set up coefficients that should be resolvable with data(sealevel).
standard <- c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF", "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1",
              "NO1", "CHI1", "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2",
              "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2",
              "ETA2", "MO3", "M3", "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
              "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")
unresolvable <- c("SA", "PI1", "S1", "PSI1", "GAM2", "H1", "H2", "T2", "R2")
resolvable <- standard[!(standard %in% unresolvable)]


test_that("tidem summaries work and constituents match previous versions", {
          m <- tidem(sealevel)
          summary(m)
          summary(m, p=0.05)
          summary(m, constituent=c("M2", "S2"))
          plot(m)
          expect_equal(length(m@data$name), 60)
          expect_equal(head(m@data$name), c("Z0", "SSA", "MSM", "MM", "MSF", "MF"))
          expect_equal(tail(m@data$name), c("2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")) 
          expect_equal(head(m@data$amplitude), c(0.98172602694827626, 0.02311206762504182, 0.00140006225693646,
                                                 0.00663853819693071, 0.00745395229070977, 0.01084231305586707))
          expect_equal(tail(m@data$amplitude), c(0.002737273208734281, 0.001037160095535379, 0.000957883534766690,
                                                 0.000475113012416056, 0.001148646040936852, 0.000342828679028158))
          expect_equal(head(m@data$phase), c(0.000000000000, 206.139498621345, 272.254673957525, 198.978272205829,
                                             217.916725436950, 340.144074289794))
          expect_equal(tail(m@data$phase), c(161.30385396720155, 169.88867148214487, 233.60496716292687,
                                             283.26643976667168, 107.18784950392580, 8.34477916113116))
})

test_that("Rayleigh criterion", {
          tide1 <- tidem(sealevel)
          expect_equal(tide1[["data"]]$name, resolvable)
          tide2 <- tidem(sealevel, constituents="standard")
          expect_equal(tide1[["data"]]$name, tide2[["data"]]$name)
})

test_that("tailoring of constituents", {
          ## check names; note that "Z0" goes in by default
          tide3 <- tidem(sealevel, constituents = c("M2", "K2"))
          expect_equal(tide3[["data"]]$name, c("Z0", "M2", "K2"))
          ## check that we can remove constituents
          tide5 <- tidem(sealevel, constituents = c("standard", "-M2")) 
          expect_equal(tide5[["data"]]$name, resolvable[resolvable != "M2"])
})

