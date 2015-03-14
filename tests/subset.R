## Tests of subsetting, which was seen to be a problem in issue 563. The tests
## here are all for CTD objects, although it would be sensible to test other
## objects too.

library(oce)


make_ctd <- function(start, end) {
    p <- seq(0, 100, 1)
    S <- 35 + p / 100
    T <- 20 - p / 100
    d <- as.ctd(S, T, p)
    d <- subset(d, start <= pressure & pressure <= end)
}
focus <- c(20, 30)
d <- make_ctd(focus[1], focus[2])
stopifnot(all.equal.numeric(d[["pressure"]], 20:30))
rm(list=ls())

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
stopifnot(all.equal.numeric(d[["pressure"]], 20:30))
rm(list=ls())


start <- 20
end <- 30
p <- seq(0, 100, 1)
S <- 35 + p / 100
T <- 20 - p / 100
d <- as.ctd(S, T, p)
d <- subset(d, start <= pressure & pressure <= end)
stopifnot(all.equal.numeric(d[["pressure"]], 20:30))
rm(list=ls())


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
stopifnot(all.equal.numeric(d[["pressure"]], 20:30))
rm(list=ls())


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
stopifnot(all.equal.numeric(d[["pressure"]], 20:30))
rm(list=ls())

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
stopifnot(all.equal.numeric(d[["pressure"]], 20:30))

