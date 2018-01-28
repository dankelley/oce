## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

## Tests of subsetting, which was seen to be a problem in issue 563. The tests
## here are all for CTD objects, although it would be sensible to test other
## objects too.

library(oce)

context("subset")

test_that("subset a CTD by pressure (1)", {
          make_ctd <- function(start, end) {
              p <- seq(0, 100, 1)
              S <- 35 + p / 100
              T <- 20 - p / 100
              d <- as.ctd(S, T, p)
              d <- subset(d, start <= pressure & pressure <= end)
          }
          focus <- c(20, 30)
          d <- make_ctd(focus[1], focus[2])
          expect_equal(d[["pressure"]], 20:30)
})

test_that("subset a CTD by pressure (2)", {
          make_ctd <- function(start, end) {
              start <- 20
              end <- 30
              p <- seq(0, 100, 1)
              S <- 35 + p / 100
              T <- 20 - p / 100
              d <- as.ctd(S, T, p)
              d <- subset(d, start <= pressure & pressure <= end)
          }
          focus <- c(20, 30)
          d <- make_ctd(focus[1], focus[2])
          expect_equal(d[["pressure"]], 20:30)
})

test_that("subset a CTD by pressure (3)", {
          start <- 20
          end <- 30
          p <- seq(0, 100, 1)
          S <- 35 + p / 100
          T <- 20 - p / 100
          d <- as.ctd(S, T, p)
          d <- subset(d, start <= pressure & pressure <= end)
          expect_equal(d[["pressure"]], 20:30)
})

test_that("subset a CTD by pressure (4)", {
          wrapper <- function(start, end) {
              make_ctd <- function(start, end) {
                  p <- seq(0, 100, 1)
                  S <- 35 + p / 100
                  T <- 20 - p / 100
                  d <- as.ctd(S, T, p)
                  d <- subset(d, start <= pressure & pressure <= end)
              }
              make_ctd(start, end)
          }
          focus <- c(20, 30)
          d <- wrapper(focus[1], focus[2])
          expect_equal(d[["pressure"]], 20:30)
})

test_that("subset a CTD by pressure (5)", {
          outerWrapper <- function(start, end) {
              wrapper <- function(start, end) {
                  make_ctd <- function(start, end) {
                      p <- seq(0, 100, 1)
                      S <- 35 + p / 100
                      T <- 20 - p / 100
                      d <- as.ctd(S, T, p)
                      d <- subset(d, start <= pressure & pressure <= end)
                  }
                  make_ctd(start, end)
              }
              wrapper(start, end)
          }
          focus <- c(20, 30)
          d <- outerWrapper(focus[1], focus[2])
          expect_equal(d[["pressure"]], 20:30)
})

test_that("subset a CTD by pressure (6)", {
          make_ctd <- function(start, end) {
              start <- 20
              end <- 30
              p <- seq(0, 100, 1)
              S <- 35 + p / 100
              T <- 20 - p / 100
              d <- as.ctd(S, T, p)
              d <- subset(d, start <= pressure & pressure <= end)
          }
          focus <- c(1, 100)
          d <- make_ctd(focus[1], focus[2])
          expect_equal(d[["pressure"]], 20:30)
})

