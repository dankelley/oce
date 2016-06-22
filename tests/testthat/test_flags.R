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

test_that("handleFLags works with argo data", {
          data(argo)
          argoNew <- handleFlags(argo, flags=list(salinity=4:5))
          ##print(head(data.frame(oldS=argo[['salinity']], flag=argo[['salinityFlag']], new=argoNew[['salinity']])))
          ##cat("argo salinity: orig had", sum(is.na(argo[['salinity']])), "NA values; new has",
          ##    sum(is.na(argoNew[['salinity']])), "\n")
          expect_equal(sum(is.na(argo[["salinity"]])), 90)
          expect_equal(sum(is.na(argoNew[["salinity"]])), 110)
          ## test replacement via function
          f <- function(object) rep(30, length.out=length(object[['salinity']]))
          argoNew2 <- handleFlags(argo, flags=list(salinity=4:5), actions=list(salinity=f))
          expect_equal(sum(argoNew2[['salinity']]==30, na.rm=TRUE),
                       sum(argo[['salinityFlag']] == 4 | argo[['salinityFlag']] == 5, na.rm=TRUE))
})

