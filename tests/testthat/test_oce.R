## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
## test oce.R

library(oce)

context("oce")

test_that("as.oce", {
          d <- data.frame(x=seq(0,1,length.out=20), y=seq(0,100,length.out=20))
          o <- as.oce(d)
          S <- seq(30,35,length.out=10)
          T <- seq(20,10,length.out=10)
          p <- seq(1,100,length.out=10)
          ctd1 <- as.oce(list(salinity=S, temperature=T, pressure=p))
          ctd2 <- as.oce(data.frame(salinity=S, temperature=T, pressure=p))
          expect_equal(ctd1[["data"]], ctd2[["data"]])
          cl <- as.oce(data.frame(longitude=c(1,2,1), latitude=c(0,1,0)))
          expect_equal(cl[['longitude']], c(1,2,1))
          expect_equal(cl[['latitude']], c(0,1,0))
})

test_that("head_adp", {
          data(adp)
          h <- head(adp)
          h3 <- head(adp, 3)
          expect_equal(h[["time"]], head(adp[["time"]]))
          expect_equal(dim(h[["v"]]), c(6, 84,  4))
          expect_equal(dim(h3[["v"]]), c(3, 84,  4))
})

test_that("head_adv", {
          data(adv)
          h <- head(adv)
          h3 <- head(adv, 3)
          expect_equal(h[["time"]], head(adv[["time"]]))
          expect_equal(dim(h[["v"]]), c(6, 3))
          expect_equal(dim(h3[["v"]]), c(3, 3))
})

test_that("head_argo", {
          data(argo)
          ## This test is hard to read, because it is exhaustive, and the data
          ## format for argo objects is tricky, with vectors, matrices, time
          ## vectors that R does not regard as vectors (why?) and character
          ## strings.
          for (n in c(3, -3)) {
            h <- head(argo, n)
            for (name in names(argo@metadata)) {
              if (name %in% c("direction", "juldQc", "positionQc")) {
                ## select characters in a string
                j <- head(seq_len(nchar(argo@metadata[[name]])), n)
                expect_equal(h@metadata[[name]],
                             substr(argo@metadata[[name]], j[1], tail(j, 1)))
              } else if (name == "flags") {
                j <- head(seq_len(dim(argo@metadata$flags[[1]])[2]), n)
                for (fname in names(argo@metadata$flags)) {
                  expect_equal(h@metadata$flags[[fname]], argo@metadata$flags[[fname]][, j])
                }
              } else if (is.vector(argo@metadata[[name]])) {
                expect_equal(h@metadata[[name]], head(argo@metadata[[name]], n))
              } else if (is.matrix(argo@metadata[[name]])) {
                j <- head(seq_len(dim(argo@metadata[[name]])[2]), n)
                expect_equal(h@metadata[[name]], argo@metadata[[name]][, j])
              } else {
                warning("ignoring metadata item: '", name, "'")
              }
            }
            for (name in names(argo@data)) {
              if (is.vector(argo@data[[name]])) {
                expect_equal(h@data[[name]], head(argo@data[[name]], n))
              } else if (is.matrix(argo@data[[name]])) {
                j <- head(seq_len(dim(argo@data[[name]])[2]), n)
                expect_equal(h@data[[name]], argo@data[[name]][, j])
              } else if (name == "time") {
                ## for reasons unknown, time is not a vector
                expect_equal(h@data[[name]], head(argo@data[[name]], n))
              } else {
                warning("ignoring data item: '", name, "'")
              }
            }
          }
})

test_that("head_ctd", {
          data(ctd)
          h <- head(ctd)
          h3 <- head(ctd, 3)
          expect_equal(length(h[["salinity"]]), 6L)
          expect_equal(length(h3[["salinity"]]), 3L)
          expect_equal(h[["salinity"]], head(ctd[["salinity"]], 6L))
          expect_equal(h3[["salinity"]], head(ctd[["salinity"]], 3L))
})

test_that("head_coastline", {
          data(coastlineWorld)
          for (n in c(-3, 3)) {
            h <- head(coastlineWorld, n)
            expect_equal(h[["longitude"]], head(coastlineWorld[["longitude"]], n))
            expect_equal(h[["latitude"]], head(coastlineWorld[["latitude"]], n))
          }
})

test_that("head_section", {
          data(section)
          s <- head(section)
          s3 <- head(section, 3)
          expect_equal(s@metadata$sectionId, head(section@metadata$sectionId, 6))
          expect_equal(s@metadata$longitude, head(section@metadata$longitude, 6))
          expect_equal(s@metadata$latitude, head(section@metadata$latitude, 6))
          expect_equal(s@metadata$time, head(section@metadata$time, 6))
          expect_equal(s3@metadata$sectionId, head(section@metadata$sectionId, 3))
          expect_equal(s3@metadata$longitude, head(section@metadata$longitude, 3))
          expect_equal(s3@metadata$latitude, head(section@metadata$latitude, 3))
          expect_equal(s3@metadata$time, head(section@metadata$time, 3))
          expect_equal(s@data$station, head(section@data$station, 6))
          expect_equal(s3@data$station, head(section@data$station, 3))
})

test_that("oceApprox", {
          ## Test for same values after rewriting the C code in C++.
          d <- data.frame(x=seq(0, 1, length.out=20), y=seq(0, 100, length.out=20))
          da <- oceApprox(d$x, d$y, c(0.4, 0.5, 0.6))
          expect_equal(da, c(40, 50, 60))
          if (require(ocedata)) {
            data(RRprofile, package="ocedata")
            zz <- seq(0, 2000, 2)
            a1 <- oce.approx(RRprofile$depth, RRprofile$temperature, zz, "rr")
            a2 <- oce.approx(RRprofile$depth, RRprofile$temperature, zz, "unesco")
            expect_equal(head(a1), c(2.95, 2.95, 2.95, 2.95, 2.95, 2.95))
            expect_equal(tail(a1), c(3.491641285, 3.490851919, 3.490063336,
                                     3.489275206, 3.488487181, 3.487698885))
            expect_equal(head(a2), c(2.95, 2.95, 2.95, 2.95, 2.95, 2.95))
            expect_equal(tail(a2), c(3.517629418, 3.516250649, 3.514868001,
                                     3.513481474, 3.512091068, 3.487698885))
          }
})

test_that("tail_adp", {
          ## This test is hard to read, because it is exhaustive, and the data
          ## format for argo objects is tricky, with vectors, matrices, time
          ## vectors that R does not regard as vectors (why?) and character
          ## strings.
          for (n in c(3, -3)) {
            h <- tail(argo, n)
            for (name in names(argo@metadata)) {
              if (name %in% c("direction", "juldQc", "positionQc")) {
                ## select characters in a string
                j <- tail(seq_len(nchar(argo@metadata[[name]])), n)
                expect_equal(h@metadata[[name]],
                             substr(argo@metadata[[name]], j[1], tail(j, 1)))
              } else if (name == "flags") {
                j <- tail(seq_len(dim(argo@metadata$flags[[1]])[2]), n)
                for (fname in names(argo@metadata$flags)) {
                  expect_equal(h@metadata$flags[[fname]], argo@metadata$flags[[fname]][, j])
                }
              } else if (is.vector(argo@metadata[[name]])) {
                expect_equal(h@metadata[[name]], tail(argo@metadata[[name]], n))
              } else if (is.matrix(argo@metadata[[name]])) {
                j <- tail(seq_len(dim(argo@metadata[[name]])[2]), n)
                expect_equal(h@metadata[[name]], argo@metadata[[name]][, j])
              } else {
                warning("ignoring metadata item: '", name, "'")
              }
            }
            for (name in names(argo@data)) {
              if (is.vector(argo@data[[name]])) {
                expect_equal(h@data[[name]], tail(argo@data[[name]], n))
              } else if (is.matrix(argo@data[[name]])) {
                j <- tail(seq_len(dim(argo@data[[name]])[2]), n)
                expect_equal(h@data[[name]], argo@data[[name]][, j])
              } else if (name == "time") {
                ## for reasons unknown, time is not a vector
                expect_equal(h@data[[name]], tail(argo@data[[name]], n))
              } else {
                warning("ignoring data item: '", name, "'")
              }
            }
          }
})

test_that("tail_adv", {
          data(adv)
          h <- tail(adv)
          h3 <- tail(adv, 3)
          expect_equal(h[["time"]], tail(adv[["time"]]))
          expect_equal(dim(h[["v"]]), c(6, 3))
          expect_equal(dim(h3[["v"]]), c(3, 3))
})

test_that("tail_argo", {
          data(argo)
          t <- tail(argo)
          ## FIXME: there are a LOT of things that ought to be tested
          ## in addition to the below. And some of the are really quite
          ## tricky, because items may be (a) in a vector, (b) in a matrix,
          ## (c) in a time vector, which R doesn't call a vector or (d)
          ## in the characters within a string. Checking for all of these
          ## is daunting, and basically means reprroducing the code
          ## that does tail.oce(). I prefer to just check the main things
          ## for now, doing the rest by eye on the sample dataset.
          expect_equal(t[["time"]], tail(argo[["time"]]))
          expect_equal(t[["longitude"]], tail(argo[["longitude"]]))
          expect_equal(t[["latitude"]], tail(argo[["latitude"]]))
          t3 <- tail(argo, 3)
          expect_equal(t3[["time"]], tail(argo[["time"]], 3))
          expect_equal(t3[["longitude"]], tail(argo[["longitude"]], 3))
          expect_equal(t3[["latitude"]], tail(argo[["latitude"]], 3))
})

test_that("tail_ctd", {
          data(ctd)
          t <- tail(ctd)
          t3 <- tail(ctd, 3)
          expect_equal(length(t[["salinity"]]), 6)
          expect_equal(length(t3[["salinity"]]), 3)
          expect_equal(t[["salinity"]], tail(ctd[["salinity"]], 6))
          expect_equal(t3[["salinity"]], tail(ctd[["salinity"]], 3))
})

test_that("tail_coastline", {
          data(coastlineWorld)
          for (n in c(-3, 3)) {
            t <- tail(coastlineWorld, n)
            expect_equal(t[["longitude"]], tail(coastlineWorld[["longitude"]], n))
            expect_equal(t[["latitude"]], tail(coastlineWorld[["latitude"]], n))
          }
})

test_that("tail_section", {
          data(section)
          s <- tail(section)
          s3 <- tail(section, 3)
          expect_equal(s@metadata$sectionId, tail(section@metadata$sectionId, 6))
          expect_equal(s@metadata$longitude, tail(section@metadata$longitude, 6))
          expect_equal(s@metadata$latitude, tail(section@metadata$latitude, 6))
          expect_equal(s@metadata$time, tail(section@metadata$time, 6))
          expect_equal(s@data$station, tail(section@data$station, 6))
          expect_equal(s3@metadata$sectionId, tail(section@metadata$sectionId, 3))
          expect_equal(s3@metadata$longitude, tail(section@metadata$longitude, 3))
          expect_equal(s3@metadata$latitude, tail(section@metadata$latitude, 3))
          expect_equal(s3@metadata$time, tail(section@metadata$time, 3))
          expect_equal(s3@data$station, tail(section@data$station, 3))
})

test_that("trim_ts", {
   x <- seq(0, 10, 0.1)
   xlim <- c(2.0, 2.9)
   oce:::trim_ts(x, xlim, 0)
   expect_equal(oce:::trim_ts(x, xlim, 0), list(from=20, to=31))
})

