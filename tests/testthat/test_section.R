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
          expect_equal(length(section@data$station), length(section@data$station))
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
          expect_warning(sec <- as.section(ctds), "estimated waterDepth as max\\(pressure\\) for CTDs numbered: 1 2")
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
          ss <- sectionSort(section)
})
