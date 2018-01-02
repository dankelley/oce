## vim:textwidth=120:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(sealevel)

rms <- function(x) sqrt(mean(x^2, na.rm=TRUE))

context("tidem")

## Set up coefficients that should be resolvable with data(sealevel).
standard <- c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF", "ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1",
              "NO1", "CHI1", "PI1", "P1", "S1", "K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2",
              "2N2", "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2",
              "ETA2", "MO3", "M3", "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
              "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")
unresolvable <- c("SA", "PI1", "S1", "PSI1", "GAM2", "H1", "H2", "T2", "R2")
resolvable <- standard[!(standard %in% unresolvable)]


test_that("tidem constituents match previous versions", {
          m <- tidem(sealevel)
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

test_that("Foreman (1977 App 7.3) and T-TIDE (Pawlowciz 2002 Table 1) test", {
          foreman <- read.table("tide_foreman.dat.gz", header=TRUE, stringsAsFactors=FALSE)
          ttide <- read.table("tide_ttide.dat.gz", skip=9, header=TRUE, stringsAsFactors=FALSE)
          ## switch T_TIDE to Foreman names (which tidem() also uses)
          ttide$name <- gsub("^MS$", "M8", gsub("^UPSI$", "UPS1", ttide$name))
          expect_equal(ttide$name, foreman$name)
          expect_equal(ttide$frequency, foreman$frequency, tol=5e-6) # T_TIDE reports to 1e-5
          ## Fit a tidal model, with an added constituent and two inferred constituents;
          ## this is set up to matche the test in Foreman's Appendix 7.1 (and 7.3),
          ## and also in the TTIDE paper by Pawlowicz et al 2002 (Table 1).
          data("sealevelTuktoyaktuk")
          m <- tidem(sealevelTuktoyaktuk, constituents=c("standard", "M10"),
                     infer=list(name=c("P1", "K2"), # 0.0415525871 0.0835614924
                                from=c("K1", "S2"), # 0.0417807462 0.0833333333
                                amp=c(0.33093, 0.27215),
                                phase=c(-7.07, -22.40)))
          ## Do constituents match Foreman and TTIDE?
          expect_equal(foreman$name, m@data$name)
          expect_equal(foreman$frequency, m@data$freq, tol=1e-7)
          expect_equal(ttide$name, m@data$name)
          expect_equal(ttide$frequency, m@data$freq, tol=5e-6) # reported to 1e-5
          ## Did the inference formulae work? (Does not address whether results are correct!)
          expect_equal(0.33093,
                       m[["amplitude"]][which(m[["name"]]=="P1")]/m[["amplitude"]][which(m[["name"]]=="K1")])
          expect_equal(m[["phase"]][which(m[["name"]]=="P1")],
                       m[["phase"]][which(m[["name"]]=="K1")]-(-7.07))
          expect_equal(0.27215,
                       m[["amplitude"]][which(m[["name"]]=="K2")]/m[["amplitude"]][which(m[["name"]]=="S2")])
          expect_equal(m[["phase"]][which(m[["name"]]=="K2")],
                       m[["phase"]][which(m[["name"]]=="S2")]-(-22.40))
          ## There seem to be some errors in Foreman's 1977 appendix 7.3,
          ## e.g. the frequency listed for 2SK5 is 0.20844743 in
          ## appendix 7.3 (p58), but in appendix 7.1 (p41) it is listed
          ## as 0.2084474129. This does not seem to be a rounding error,
          ## so the cause is a bit of a mystery. In any case, this
          ## explains why the tolerance is relaxed in the next
          ## comparison.
          ## BUG: the next should work but it does not (issue 1351)
          ## > expect_equal(foreman$A, m[["amplitude"]], tol=1e-4)
          ## Error: foreman$A not equal to m[["amplitude"]].
          ## 4/39 mismatches (average diff: 0.00219)
          ## [9]  0.0465 - 0.0446 ==  0.001910
          ## [10] 0.1406 - 0.1347 ==  0.005858
          ## [19] 0.2195 - 0.2202 == -0.000737
          ## [20] 0.0597 - 0.0599 == -0.000237
})

