library(oce)
context("Flags")

test_that("handleFLags works with ctd data", {
          data(section)
          ctd <- section[["station", 100]]
          ## this stn has a few points with salinityFlag==3
          ctdNew <- handleFlags(ctd, flags=list(salinity=c(1, 3:9)))
          ##cat("ctd salinity: orig had", sum(is.na(ctd[['salinity']])), "NA values; new has",
          ##    sum(is.na(ctdNew[['salinity']])), "\n")
          expect_equal(sum(is.na(ctd[["salinity"]])), 0)
          expect_equal(sum(is.na(ctdNew[["salinity"]])), 2)
})

test_that("handleFLags works with argo data", {
          data(argo)
          argoNew <- handleFlags(argo, flags=list(salinity=4:5))
          ##print(head(data.frame(oldS=argo[['salinity']], flag=argo[['salinityFlag']], new=argoNew[['salinity']])))
          ##cat("argo salinity: orig had", sum(is.na(argo[['salinity']])), "NA values; new has",
          ##    sum(is.na(argoNew[['salinity']])), "\n")
          expect_equal(sum(is.na(argo[["salinity"]])), 90)
          expect_equal(sum(is.na(argoNew[["salinity"]])), 110)
})

