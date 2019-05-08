## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

## Many of these tests will fail if data(section) is changed. This is on
## purpose, because changing a long-standing dataset is to be avoided!

context("section")

test_that("data(section) has not altered", {
          data(section)
          expect_equal(124, length(section[["station"]]))
          expect_equal(124, length(section@data$station))
})

test_that("section[['z']] == -section[['depth']]", {
          data(section)
          z <- section[["z"]]
          depth <- section[["depth"]]
          expect_equal(z, -depth)
})

test_that("section[[...]] and [[..., \"byStation\"]] work", {
          data(section)
          for (i in c("CT", "depth", "nitrate", "nitrite", "oxygen",
                      "phosphate", "potential temperature", "pressure", "SA",
                      "salinity", "sigmaTheta", "silicate", "spice",
                      "temperature", "theta", "z")) {
            v <- section[[i]]
            expect_true(is.vector(v))
            expect_equal(length(v), 2841)
            l <- section[[i, "byStation"]]
            expect_true(is.list(l))
            expect_equal(head(v, 5), l[[1]])
            expect_equal(length(l), 124)
          }
})

test_that("as.section() data-quality flags", {
          data(section)
          ## The below is also in ../../create_data/section/check_section.R, and it would be
          ## smart to update both at the same time.
          stn2 <- section[['station', 2]]
          twos <- rep(2, 16)
          ## there are no flags on temperature or pressure
          expect_equal(stn2[["startTime"]], as.POSIXct("1993-09-24 00:13:00", tz="UTC"))
          expect_equal(stn2@metadata$flags$salinity, c(2,2,2,2,3,3,2,2,3,3,3,3,3,3,2,2))
          expect_equal(stn2@metadata$flags$salinityBottle, c(2,3,2,2,2,3,2,2,2,2,2,2,2,2,2,2))
          expect_equal(stn2@metadata$flags$oxygen, twos)
          expect_equal(stn2@metadata$flags$silicate, twos)
          expect_equal(stn2@metadata$flags$nitrite, twos)
          expect_equal(stn2@metadata$flags[["NO2+NO3"]], twos)
          expect_equal(stn2@metadata$flags$phosphate, c(2,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2))
          ## The next ensures the correct interpretation of the missing value
          ## numbers in the file.
          expect_equal(section[['station',15]][['nitrite']],
                       c(0.00, 0.00, 0.05, 0.00, 0.00, 0.00, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
})

test_that("section station extraction", {
          data(section)
          expect_equal(length(section@data$station), length(section[["station"]]))
          expect_equal(section[["station", 1]][["station"]], "3")
          expect_equal(section[["station", "103"]][["station"]], "103")
})


test_that("as.section() works with names of CTD objects", {
          data(ctd)
          fake <- ctd
          fake[["temperature"]] <- ctd[["temperature"]] + 0.5
          fake[["salinity"]] <- ctd[["salinity"]] + 0.1
          fake[["longitude"]] <- ctd[["longitude"]] + 0.01
          fake[["station"]] <- "fake"
          sec <- as.section(c("ctd", "fake"))
          expect_equal(2, length(sec[["station"]]))
})

test_that("as.section() works with vector of CTD objects", {
          data(ctd)
          ctds <- vector("list", 2)
          ctds[[1]] <- ctd
          fake <- ctd
          fake[["temperature"]] <- ctd[["temperature"]] + 0.5
          fake[["salinity"]] <- ctd[["salinity"]] + 0.1
          fake[["longitude"]] <- ctd[["longitude"]] + 0.01
          fake[["station"]] <- "fake"
          ctds[[2]] <- fake
          expect_warning(sec <- as.section(ctds), "estimated waterDepth as max\\(pressure\\) for CTDs numbered 1:2")
          expect_equal(2, length(sec[["station"]]))
})

test_that("as.section() works with argo object", {
          data("argo")
          sec <- as.section(subset(argo, profile < 10))
          expect_equal(9, length(sec[["station"]]))
})

test_that("subset(section, indices=(NUMERIC))", {
          data(section)
          sec2 <- subset(section, indices=3:6)
          expect_equal(4, length(sec2[["station"]]))
          expect_true(identical(sec2[["station", 1]], section[["station", 3]]))
})

test_that("subset(section, indices=(LOGICAL))", {
          data(section)
          long <- subset(section,
                         indices=unlist(lapply(section[["station"]], function(s) 10<length(s[["pressure"]]))))
          expect_equal(120, length(long[["station"]]))
          expect_equal(section[["station",2]], long[["station",1]])
})

test_that("subset(section, longitude < (NUMERIC))", {
          data(section)
          secWest <- subset(section, longitude < -50)
          expect_lt(max(secWest[["longitude"]]), -50)
})

test_that("subset(section, pressure < 2000)", {
          data(section)
          top2km <- subset(section, pressure < 2000) # drops stn 56 and 62
          section100 <- section[["station", 100]]
          top2km98 <- top2km[["station", 98]]
          expect_equal(tail(section100[["pressure"]]), c(3530.9, 3746.1, 3970.6, 4189.5, 4346.7, 4398.3))
          expect_equal(tail(top2km98[["pressure"]]), c(777.1, 926.5, 1189.5, 1590.1, 1699.8, 1859.5))
})

test_that("subset(section, pressure > 1000)", {
          data(section)
          deep <- subset(section, pressure > 1000) # drops stn 1, 2, 123, 124
          w <- which(section[["station", 100]][["pressure"]] > 1000)
          d <- data.frame(section[["station", 100]][["data"]])[w, ]
          rownames(d) <- 1:dim(d)[1]
          expect_equal(d, as.data.frame(deep[["station", 98]][["data"]]))
})

test_that("subset(section, min(pressure)<100)", {
          data(section)
          SEC <- subset(section, min(pressure) < 100)
          ptop <- unlist(lapply(section[["station"]],
                                function(s) min(s[["pressure"]])))
          bad <- sum(ptop >= 100)
          expect_equal(length(SEC[["station"]]), length(section[["station"]]) - bad)
})

test_that("subset(section, length(pressure) > 5)", {
          data(section)
          SEC <- subset(section, length(pressure) > 5)
          plen <- unlist(lapply(section[["station"]],
                                function(s) length(s[["pressure"]])))
          bad <- sum(plen <= 5)
          expect_equal(length(SEC[["station"]]), length(section[["station"]]) - bad)
})


test_that("sectionSort", {
          data(section)
          expect_equal(section[["stationId"]],
                       c("3", "4", "6", "7", "8", "9", "10", "12", "13", "14",
                         "15", "16", "17", "18", "19", "20", "21", "22", "23",
                         "24", "25", "26", "28", "29", "30", "32", "33", "34",
                         "35", "36", "37", "38", "39", "40", "41", "42", "43",
                         "44", "45", "46", "47", "48", "49", "50", "51", "52",
                         "53", "54", "55", "56", "57", "58", "59", "60", "61",
                         "62", "63", "64", "65", "66", "67", "68", "69", "71",
                         "72", "74", "75", "76", "77", "78", "79", "80", "81",
                         "82", "83", "84", "85", "86", "87", "88", "89", "90",
                         "91", "92", "93", "94", "95", "96", "97", "98", "99",
                         "100", "101", "102", "103", "104", "106", "107", "108",
                         "109", "110", "111", "112", "113", "114", "115", "116",
                         "117", "118", "119", "120", "121", "122", "123", "124",
                         "125", "126", "127", "128", "129", "130", "131", "132",
                         "133"))
          ss <- sectionSort(section)
          expect_equal(ss[["stationId"]],
                       c("10", "100", "101", "102", "103", "104", "106", "107",
                         "108", "109", "110", "111", "112", "113", "114", "115",
                         "116", "117", "118", "119", "12", "120", "121", "122",
                         "123", "124", "125", "126", "127", "128", "129", "13",
                         "130", "131", "132", "133", "14", "15", "16", "17",
                         "18", "19", "20", "21", "22", "23", "24", "25",
                         "26", "28", "29", "3", "30", "32", "33", "34",
                         "35", "36", "37", "38", "39", "4", "40", "41",
                         "42", "43", "44", "45", "46", "47", "48", "49",
                         "50", "51", "52", "53", "54", "55", "56", "57",
                         "58", "59", "6", "60", "61", "62", "63", "64",
                         "65", "66", "67", "68", "69", "7", "71", "72",
                         "74", "75", "76", "77", "78", "79", "8", "80",
                         "81", "82", "83", "84", "85", "86", "87", "88",
                         "89", "9", "90", "91", "92", "93", "94", "95",
                         "96", "97", "98", "99"))
})

test_that("stationReplaceIndividualStation", {
          data(section)
          section[["station"]][[1]] <- "not a CTD object"
          expect_equal(section@data$station[[1]], "not a CTD object")
})

test_that("stationReplaceAllStations", {
          data(section)
          expect_false("N2" %in% names(section[["station",1]][["data"]]))
          section[["station"]] <- lapply(section[["station"]], function(x) oceSetData(x, "N2", swN2(x)))
          expect_true("N2" %in% names(section[["station",1]][["data"]]))
})

test_that("sectionGrid", {
          data(section)
          s <- subset(section, 115<=stationId&stationId<=121)
          sg <- sectionGrid(s, p=seq(0, 5000, 500))
          ## Check flag names
          expect_equal(sort(names(section[["station",1]][["flags"]]), method="radix"),
                         sort(names(sg[["station",1]][["flags"]]), method="radix"))
          ## Check units
          expect_equal(section[["station",1]][["units"]], sg[["station",1]][["units"]])
})

test_that("sectionSmooth", {
          data(section)
          s <- subset(section, 115<=stationId&stationId<=121)
          sg <- sectionGrid(s, p=seq(0, 5000, 500))
          sspline <- sectionSmooth(sg, "spline")
          expect_equal(length(s[["station"]]), length(sspline[["station"]]))
          ## Check flag names
          expect_equal(sort(names(sspline[["station",1]][["flags"]]), method="radix"),
                       sort(names(section[["station",1]][["flags"]]), method="radix"))
          ## Check dimensionality when xg is given
          sspline2 <- sectionSmooth(sg, "spline", xg=seq(0,200,50))
          expect_equal(length(sspline2[["station"]]), 5)
          sbarnes <- sectionSmooth(s, "barnes", xr=50, yr=200)
          expect_equal(length(s[["station"]]), length(sbarnes[["station"]]))
          if (requireNamespace("automap", quietly=TRUE) &&
              requireNamespace("sp", quietly=TRUE)) {
            expect_warning(skrigingInternal <- sectionSmooth(s, "kriging"), "NaNs produced")
            expect_equal(length(s[["station"]]), length(skrigingInternal[["station"]]))
            expect_warning(skrigingInternal2 <- sectionSmooth(s, "kriging", xg=seq(0,200,50)), "NaNs produced")
            expect_equal(length(skrigingInternal2[["station"]]), 5)
            krigFunction <- function(x, y, F, xg, xr, yg, yr) {
              xy <- data.frame(x=x/xr, y=y/yr)
              K <- automap::autoKrige(F~1, remove_duplicates=TRUE,
                                      input_data=sp::SpatialPointsDataFrame(xy, data.frame(F)),
                                      new_data=sp::SpatialPoints(expand.grid(xg/xr, yg/yr)))
              matrix(K$krige_output@data$var1.pred, nrow=length(xg), ncol=length(yg))
            }
            expect_warning(skrigingUser <- sectionSmooth(s, krigFunction), "NaNs produced")
            expect_equal(length(skrigingUser[["station"]]), length(s[["station"]]))
            expect_warning(skrigingUser2 <- sectionSmooth(s, krigFunction, xg=seq(0,200,50)), "NaNs produced")
            expect_equal(length(skrigingUser2[["station"]]), 5)
          }
})

