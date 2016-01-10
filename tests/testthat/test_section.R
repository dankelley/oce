## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)

context("section")

test_that("data(section) has not altered", {
          data(section)
          expect_equal(124, length(section[["station"]]))
          expect_equal(124, length(section@data$station))
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


