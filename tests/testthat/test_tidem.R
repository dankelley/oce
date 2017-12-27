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

test_that("Foreman (1977 App 7.3) test", {
          app73txt <- "index name frequency AL GL
          1 Z0     0.00000000  1.9806    0.00
          2 MM     0.00151215  0.2121  288.50
          3 MSF    0.00282193  0.1561  115.15
          4 ALP1   0.03439657  0.0141  180.96
          5 2Q1    0.03570635  0.0226  246.82
          6 Q1     0.03721850  0.0144  252.75
          7 O1     0.03873065  0.0694  284.43
          8 NO1    0.04026859  0.0380  275.85
          9 P1     0.04155259  0.0468  252.20
          10 K1     0.04178075  0.1332  145.54
          11 J1     0.04329290  0.0234  103.63
          12 OO1    0.04483084  0.0463  358.47
          13 UPS1   0.04634299  0.0233  239.12
          14 EPS2   0.07617731  0.0216  109.98
          15 MU2    0.07768947  0.0428   30.06
          16 N2     0.07899925  0.0857  306.35
          17 M2     0.08051140  0.5007    4.40
          18 L2     0.08202355  0.0174  168.03
          19 S2     0.08333334  0.2193   36.74
          20 K2     0.08356149  0.0515  131.15
          21 ETA2   0.08507364  0.0059  235.38
          22 MO3    0.11924206  0.0138   11.86
          23 M3     0.12076710  0.0126  331.91
          24 MK3    0.12229215  0.0048  339.15
          25 SK3    0.12511408  0.0022  228.64
          26 MN4    0.15951066  0.0096   85.00
          27 M4     0.16102280  0.0131  145.17
          28 SN4    0.16233259  0.0085   82.78
          29 MS4    0.16384473  0.0011  176.14
          30 S4     0.16666667  0.0047  119.75
          31 2MK5   0.20280355  0.0013  244.34
          32 2SK5   0.20844743  0.0043    5.04
          33 2MN6   0.24002205  0.0038   26.46
          34 M6     0.24153420  0.0018  298.97
          35 2MS6   0.24435614  0.0059   69.59
          36 2SM6   0.24717808  0.0023   45.80
          37 3MK7   0.28331494  0.0086   73.20
          38 M8     0.32204559  0.0033  109.22
          39 M10    0.40255699  0.0010  191.71"
          app73 <- read.table(text=app73txt, header=TRUE, stringsAsFactors=FALSE)
          data("sealevelTuktoyaktuk")
          m <- tidem(sealevelTuktoyaktuk, constituents=c("standard", "P1", "K2", "M10"))
          expect_equal(app73$name, m@data$name)
          ## There seem to be some errors in Foreman's 1977 appendix 7.3,
          ## e.g. the frequency listed for 2SK5 is 0.20844743 in
          ## appendix 7.3 (p58), but in appendix 7.1 (p41) it is listed
          ## as 0.2084474129. This does not seem to be a rounding error,
          ## so the cause is a bit of a mystery. In any case, this
          ## explains why the tolerance is relaxed in the next
          ## comparison.
          expect_equal(app73$frequency, m@data$freq, tol=1e-7)

})

