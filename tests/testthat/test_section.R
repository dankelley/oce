## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

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
          expect_equal(stn2@metadata$flags$salinity, c(2,2,2,2,3,3,2,2,3,3,3,3,3,3,2,2))
          expect_equal(stn2@metadata$flags$salinity2, c(2,3,2,2,2,3,2,2,2,2,2,2,2,2,2,2))
          expect_equal(stn2@metadata$flags$oxygen, twos)
          expect_equal(stn2@metadata$flags$silicate, twos)
          expect_equal(stn2@metadata$flags$nitrate, twos)
          expect_equal(stn2@metadata$flags$nitrite, twos)
          expect_equal(stn2@metadata$flags$phosphate, c(2,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2))
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
          sec <- as.section(ctds)
          expect_equal(2, length(sec[["station"]]))
})

test_that("as.section() works with argo object", {
          data(argo)
          sec <- as.section(subset(argo, profile < 5))
          expect_equal(4, length(sec[["station"]]))
})


