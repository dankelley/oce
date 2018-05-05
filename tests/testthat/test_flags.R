library(oce)
context("Flags")

test_that("[[ and [[<- work with ctd flags", {
          data(section)
          ctd <- section[["station", 100]]
          expect_equal(c(2,2,2,2,2,3), ctd[["salinityFlag"]][1:6])
          ctd[["salinity"]][2] <- -999
          ctd[["salinityFlag"]] <- ifelse(ctd[["salinity"]] < 0, 3, ctd[["salinityFlag"]])
          expect_equal(c(2,3,2,2,2,3), ctd[["salinityFlag"]][1:6])
          ctd[["salinity"]] <- ifelse(ctd[["salinityFlag"]]!=2, NA, ctd[["salinity"]])
          expect_equal(is.na(ctd[["salinity"]][1:6]), c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE))
})

test_that("handleFLags works with ctd data", {
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

test_that("handleFLags works with the built-in argo dataset", {
          data(argo)
          argoNew <- handleFlags(argo, flags=list(salinity=4:5))
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
          argoNew2 <- handleFlags(argo)
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

test_that("handleFLags works with the built-in section dataset", {
          data(section)
          SECTION <- handleFlags(section)
          ## Inspection reveals that salinity are triggered in the first CTD entry, i.e.
          ## the station named "3" in this dataset.

          ## The default for `handleFlags,ctd-method` is the WOCE standard, with 2=good, 3=bad, ...
          stn1 <- section[["station", 1]]
          STN1 <- SECTION[["station", 1]]
          expect_equal(c(2, 3, 3, 2, 2), stn1[["salinityFlag"]])
          ok <- which(2 == stn1[["salinityFlag"]])
          expect_equal(stn1[["salinity"]][ok], STN1[["salinity"]][ok])
          replace <- which(2 != stn1[["salinityFlag"]])
          expect_equal(stn1[["salinityBottle"]][replace], STN1[["salinity"]][replace])
})

test_that("does subset() work on ctd flags? (issue 1410)", {
          data(section)
          stn <- section[["station", 100]]
          stnTopKm <- subset(stn, pressure < 1000)
          n <- length(stnTopKm[["temperature"]])
          for (flag in names(stnTopKm[["flags"]])) {
              flagName <- paste(flag, "Flag", sep="")
              expect_equal(stnTopKm[[flagName]], head(stn[[flagName]], n))
          }
})

test_that("does subset() work on odf flags? (issue 1410)", {
          file <- system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF", package="oce")
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

test_that("does subset() work on adp flags? (issue 1410)", {
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

