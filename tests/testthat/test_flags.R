library(oce)
context("Flags")

extensive <- TRUE

test_that("argument existence", {
          data(ctd)
          expect_error(initializeFlagScheme(ctd, mapping=list(unknown=1, good=2, bad=3)),
                       "must supply 'name'")
          expect_error(initializeFlagScheme(ctd, name="unknown"),
                       "must supply 'mapping' for new scheme named \"unknown\"")
})

test_that("predefined flag schemes", {
          ## DEVELOPER NOTE: keep in synch with R/AllClass.R and man-roxygen/initializeFlagScheme.R
          data(ctd)
          a <- initializeFlagScheme(ctd, "argo")
          expect_equal(a[["flagScheme"]],
                       list(name="argo",
                            mapping=list(not_assessed=0, passed_all_tests=1, probably_good=2,
                                         probably_bad=3, bad=4, averaged=7,
                                         interpolated=8, missing=9)))
          a <- initializeFlagScheme(ctd, "BODC")
          expect_equal(a[["flagScheme"]],
                       list(name="BODC",
                            mapping=list(no_quality_control=0, good=1, probably_good=2,
                                         probably_bad=3, bad=4, changed=5, below_detection=6,
                                         in_excess=7, interpolated=8, missing=9)))
          a <- initializeFlagScheme(ctd, "DFO")
          expect_equal(a[["flagScheme"]],
                       list(name="DFO",
                            mapping=list(no_quality_control=0, appears_correct=1, appears_inconsistent=2,
                                         doubtful=3, erroneous=4, changed=5,
                                         qc_by_originator=8, missing=9)))
          a <- initializeFlagScheme(ctd, "WHP bottle")
          expect_equal(a[["flagScheme"]],
                       list(name="WHP bottle",
                            mapping=list(no_information=1, no_problems_noted=2, leaking=3,
                                         did_not_trip=4, not_reported=5, discrepency=6,
                                         unknown_problem=7, did_not_trip=8, no_sample=9)))
          a <- initializeFlagScheme(ctd, "WHP CTD")
          expect_equal(a[["flagScheme"]],
                       list(name="WHP CTD",
                            mapping=list(not_calibrated=1, acceptable=2, questionable=3,
                                         bad=4, not_reported=5, interpolated=6,
                                         despiked=7, missing=9)))
})

test_that("user-created flag scheme", {
          data(ctd)
          a <- initializeFlagScheme(ctd, "myscheme", list(unknown=1, good=2, bad=3))
          expect_equal(a[["flagScheme"]], list(name="myscheme",
                                               mapping=list(unknown=1, good=2, bad=3)))
})

test_that("cannot alter existing flag scheme", {
          data(ctd)
          a <- initializeFlagScheme(ctd, "myscheme", list(unknown=1, good=2, bad=3))
          expect_warning(initializeFlagScheme(a, "WHP CTD"),
                         "cannot alter a flagScheme that is already is place")
})

test_that("ctd flag scheme action", {
          data(ctd)
          a <- initializeFlags(ctd, "temperature", 2) # 2="acceptable
          expect_warning(initializeFlags(a, "temperature", 2), "cannot re-initialize flags")
          a <- setFlags(a, "temperature", 1:3, 4) # 4="bad"
})

test_that("[[ and [[<- with ctd flags", {
          data(section)
          ctd <- section[["station", 100]]
          expect_equal(c(2,2,2,2,2,3), ctd[["salinityFlag"]][1:6])
          ctd[["salinity"]][2] <- -999
          ctd[["salinityFlag"]] <- ifelse(ctd[["salinity"]] < 0, 3, ctd[["salinityFlag"]])
          expect_equal(c(2,3,2,2,2,3), ctd[["salinityFlag"]][1:6])
          ctd[["salinity"]] <- ifelse(ctd[["salinityFlag"]]!=2, NA, ctd[["salinity"]])
          expect_equal(is.na(ctd[["salinity"]][1:6]), c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("handleFLags with ctd data", {
          data(section)
          ctd <- section[["station", 100]]
          ## this stn has a few points with salinityFlag==3
          ctdNew <- handleFlags(ctd, flags=list(salinity=c(1, 3:9)))
          ##cat("ctd salinity: orig had", sum(is.na(ctd[['salinity']])), "NA values; new has",
          ##    sum(is.na(ctdNew[['salinity']])), "\n")
          expect_equal(sum(is.na(ctd[["salinity"]])), 0)
          nbad <- sum(ctd[['salinityFlag']] != 2)
          expect_equal(2, nbad)
          ## test replacement via function
          f <- function(object) rep(30, length.out=length(object[['salinity']]))
          ctdNew2 <- handleFlags(ctd, flags=list(salinity=4:5), actions=list(salinity=f))
          expect_equal(sum(ctdNew[['salinity']]==30, na.rm=TRUE),
                       sum(ctd[['salinityFlag']] == 4 | ctd[['salinityFlag']] == 5, na.rm=TRUE))
})

## test_that("handleFLags with ctd data (negative numeric flag)", {
##           data(section)
##           ## stn 100 has a few points with salinityFlag==3
##           for (i in if(extensive)seq_along(section[["station"]]) else 100) {
##               ctd <- section[["station", i]]
##               a <- handleFlags(ctd, flags=list(salinity=c(1, 3:9)))
##               expect_error(handleFlags(ctd, flags=list(salinity=-2)),
##                            "must use initializeFlagScheme\\(\\) before using negative flags")
##               b <- initializeFlagScheme(a, "WHP bottle")
##               b <- handleFlags(b, flags=list(salinity=-2))
##               expect_equal(a[["data"]], b[["data"]])
##               c <- handleFlags(b, flags=list(salinity="-no_problems_noted"))
##               expect_equal(a[["data"]], c[["data"]])
##           }
## })

test_that("handleFLags with the built-in argo dataset", {
          data(argo)
          argoNew <- handleFlags(argo, flags=list(salinity=c(0, 3:9)))
          ## Test a few that are identified by printing some values
          ## for argo[["salinityFlag"]].
          expect_true(is.na(argoNew[["salinity"]][13, 2]))
          expect_true(is.na(argoNew[["salinity"]][53, 8]))
          ## Test whether data with salinity flag of 4 get changed to NA
          expect_true(all(is.na(argoNew[["salinity"]][4==argo[["salinityFlag"]]])))
          expect_true(!all(is.na(argoNew[["salinity"]][1==argo[["salinityFlag"]]])))
          ## Similar for temperature. First, check that it is *not* NA, with
          ## the call to handleFlags() above, which was restricted to salinity.
          expect_true(!is.na(argoNew[["temperature"]][10, 2]))
          ## Now, handle *all* the flags, and check temperature again, and also salinity.
          argoNew2 <- handleFlags(argo, flags=list(4:5))
          expect_true(is.na(argoNew2[["temperature"]][10, 2]))
          expect_true(all(is.na(argoNew2[["temperature"]][4==argo[["temperatureFlag"]]])))
          # Tests of overall numbers
          expect_equal(sum(is.na(argo[["salinity"]])), 106)
          expect_equal(sum(is.na(argoNew[["salinity"]])), 140)
          ## test replacement via function
          f <- function(object) rep(30, length.out=length(object[['salinity']]))
          argoNew3 <- handleFlags(argo, flags=list(salinity=4:5), actions=list(salinity=f))
          expect_equal(sum(argoNew3[['salinity']]==30, na.rm=TRUE),
                       sum(argo[['salinityFlag']] == 4 | argo[['salinityFlag']] == 5, na.rm=TRUE))
})

## test_that("handleFLags with the built-in argo dataset (named flags)", {
##           data(argo)
##           a <- handleFlags(argo,
##                            flags=list(salinity=c("not_assessed","probably_bad","bad",
##                                                  "averaged","interpolated","missing")))
##           b <- handleFlags(argo,
##                            flags=list(salinity=c(0, 3, 4,
##                                                  7, 8, 9)))
##           expect_equal(a[["salinity"]], b[["salinity"]])
## })

test_that("handleFLags with the built-in section dataset", {
          data(section)
          SECTION<- handleFlags(section, flags=list(salinity=c(1, 3:9)))
          ## Inspection reveals that salinity are triggered in the first CTD entry, i.e.
          ## the station named "3" in this dataset.

          ## The default for `handleFlags,ctd-method` is the WOCE standard, with 2=good, 3=bad, ...
          stn1 <- section[["station", 1]]
          STN1 <- SECTION[["station", 1]]
          expect_equal(c(2, 3, 3, 2, 2), stn1[["salinityFlag"]])
          ok <- 2 == stn1[["salinityFlag"]]
          expect_equal(!is.na(STN1[["salinity"]]), ok)
})

test_that("ctd flag with subset() (issue 1410)", {
          data(section)
          stn <- section[["station", 100]]
          stnTopKm <- subset(stn, pressure < 1000)
          n <- length(stnTopKm[["temperature"]])
          for (flag in names(stnTopKm[["flags"]])) {
              flagName <- paste(flag, "Flag", sep="")
              expect_equal(stnTopKm[[flagName]], head(stn[[flagName]], n))
          }
})

test_that("odf flag with subset() (issue 1410)", {
          file <- system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF.gz", package="oce")
          odf <- expect_warning(read.odf(file), "should be unitless")
          ## # Find a region with interesting flags
          ## > which(odf[["sigmaThetaFlag"]]!=1)
          ## [1] 110 120 121 142
          ## > which(odf[["salinityFlag"]]!=1)
          ## [1] 121
          iStart <- 100
          iEnd <- 130
          sub <- subset(odf, scan[iStart] <= scan & scan <= scan[iEnd])
          n <- length(sub[["temperature"]])
          for (name in names(sub[["flags"]])) {
              flagName <- paste(name, "Flag", sep="")
              expect_equal(sub[[flagName]], odf[[flagName]][iStart:iEnd])
          }
          for (namei in names(sub[["data"]])) {
              expect_equal(sub[[name]], odf[[name]][iStart:iEnd])
          }
})

test_that("adp flag with subset() (issue 1410)", {
          data(adp)
          v <- adp[["v"]]
          ## I'm fixing this in the 'develop' branch, which as of
          ## the moment has not merged the 'dk' branch's ability to
          ## set flags, so we do this the old fashioned way. And,
          ## what the heck, there's no harm in keeping it this way,
          ## as an extra check on things.
          f <- array(FALSE, dim=dim(v))
          updraft <- adp[["v"]][,,4] > 0
          updraft[is.na(updraft)] <- FALSE # I don't like NA flags
          for (beam in 1:4)
              f[,,beam] <- updraft
          adp[["vFlag"]] <- f
          ## Subset by distance.
          sub <- subset(adp, distance < 20)
          expect_equal(dim(sub[["v"]]), dim(sub[["vFlag"]])) # flag dim = data dim?
          look <- adp[["distance"]] < 20
          expect_equal(adp[["vFlag"]][, look, ], sub[["vFlag"]]) # flag values ok?
          ## Subset by time.
          sub <- subset(adp, time <= adp[["time"]][10])
          expect_equal(dim(sub[["v"]]), dim(sub[["vFlag"]])) # flag dim = data dim?
          look <- adp[["time"]] <= adp[["time"]][10]
          expect_equal(adp[["vFlag"]][look, , ], sub[["vFlag"]]) # flag values ok?
})

test_that("initializeFlagScheme with section", {
          data(section)
          expect_equal(section[["station", 1]][["flagScheme"]],
                       list(name="WHP bottle",
                            mapping=list(no_information=1, no_problems_noted=2, leaking=3,
                                         did_not_trip=4, not_reported=5, discrepency=6,
                                         unknown_problem=7, did_not_trip=8, no_sample=9)))
})

