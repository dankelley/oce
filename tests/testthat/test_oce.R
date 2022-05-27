# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

# test oce.R

library(oce)

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
    for (n in c(3, -3)) {
        h <- head(adp, n)
        look <- head(seq_len(dim(adp[["v"]])[1]), n)
        expect_equal(h[["time"]], adp[["time"]][look])
        expect_equal(h[["v"]], adp[["v"]][look,,])
    }
})

test_that("head_adv", {
    data(adv)
    for (n in c(3, -3)) {
        h <- head(adv, n)
        look <- head(seq_len(dim(adv[["v"]])[1]), n)
        expect_equal(h[["time"]], adv[["time"]][look])
        expect_equal(h[["v"]], adv[["v"]][look,])
    }
})

test_that("head_argo", {
            data(argo)
            # This test is hard to read, because it is exhaustive, and the data
            # format for argo objects is tricky, with vectors, matrices, time
            # vectors that R does not regard as vectors (why?) and character
            # strings.
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
                }
              }
              for (name in names(argo@data)) {
                  if (is.vector(argo@data[[name]])) {
                      expect_equal(h@data[[name]], head(argo@data[[name]], n))
                  } else if (is.matrix(argo@data[[name]])) {
                      j <- head(seq_len(dim(argo@data[[name]])[2]), n)
                      expect_equal(h@data[[name]], argo@data[[name]][, j])
                  } else if (name == "time") {
                      # NB time is not a vector
                      expect_equal(h@data[[name]], head(argo@data[[name]], n))
                  } else {
                      warning("ignoring data item: '", name, "'")
                  }
              }
            }
})

test_that("head_cm", {
    data(cm)
    for (n in c(3, -3)) {
        h <- head(cm, n)
        look <- head(seq_along(cm[["time"]]), n)
        for (n in names(cm@data)) {
            expect_equal(h[[n]], cm[[n]][look])
        }
    }
})

test_that("head_coastline", {
    data(coastlineWorld)
    for (n in c(-3, 3)) {
        h <- head(coastlineWorld, n)
        expect_equal(h[["longitude"]], head(coastlineWorld[["longitude"]], n))
        expect_equal(h[["latitude"]], head(coastlineWorld[["latitude"]], n))
    }
})

test_that("head_ctd", {
    data(section)
    ctd <- section[["station", 100]]
    for (n in c(-10, 10)) {
        h <- head(ctd, n)
        for (name in names(ctd@data)) {
            expect_equal(h@data[[name]], head(ctd@data[[name]], n))
            expect_equal(h@metadata$flags[[name]], head(ctd@metadata$flags[[name]], n))
        }
    }
})

test_that("head_echosounder", {
    data(echosounder)
    for (n in c(-10, 10)) {
        h <- head(echosounder, n=n)
        look <- head(seq_len(dim(echosounder[["a"]])[1]), n)
        expect_equal(h[["depth"]], echosounder[["depth"]])
        expect_equal(h[["latitude"]], echosounder[["latitude"]][look])
        expect_equal(h[["longitude"]], echosounder[["longitude"]][look])
        expect_equal(h[["time"]], echosounder[["time"]][look])
        expect_equal(h[["a"]], echosounder[["a"]][look, ])
    }
})

ladpText <- "Filename    = processed/025/025
Date        = 2014/ 7/ 6
Start_Time  = 19: 7:12
Start_Lat   = 48°N 58.4580'
Start_Lon   =  45°W 52.3932'
Deviation   = -17.791079
Columns     = z:u:v:ev
   9.9  0.266  0.034  0.050
  19.8  0.266  0.034  0.050
  29.6  0.266  0.034  0.049
  39.5  0.240  0.056  0.032
  49.4  0.222  0.063  0.027
  59.3  0.218  0.068  0.025
  69.2  0.227  0.079  0.025
  79.0  0.217  0.092  0.023
  88.9  0.204  0.098  0.023
  98.8  0.198  0.103  0.021
 108.7  0.182  0.103  0.020
 118.6  0.172  0.083  0.020
 128.5  0.174  0.089  0.019
 138.3  0.172  0.093  0.020
 148.2  0.165  0.098  0.019"

 test_that("head_ladcp", {
     dat <- read.table(text=ladpText, skip=7, header=FALSE, col.names=c("z","u","v","ev"))
     ladp <- as.ladp(longitude=48+58.4580/60, latitude=-(45+52.3932/60),
         station=25, time=as.POSIXct("2014/7/6",tz="UTC"),
         pressure=dat$z, u=dat$u, v=dat$v)
     for (n in c(3, -3)) {
         t <- head(ladp, n)
         look <- head(seq_along(ladp[["pressure"]]), n)
         for (n in names(ladp@data)) {
             expect_equal(t[[n]], ladp[[n]][look])
         }
     }
})

 test_that("head_lobo", {
     data(lobo)
     for (n in c(3, -3)) {
         h <- head(lobo, n)
         look <- head(seq_along(lobo[["time"]]), n)
         for (n in names(lobo@data)) {
             expect_equal(h[[n]], lobo[[n]][look])
         }
     }
})

 test_that("head_rsk", {
     data(rsk)
     for (n in c(3, -3)) {
         h <- head(rsk, n)
         look <- head(seq_along(rsk[["time"]]), n)
         expect_equal(h[["time"]], rsk[["time"]][look])
         expect_equal(h[["elevation"]], rsk[["elevation"]][look])
     }
})

 test_that("head_sealevel", {
     data(sealevel)
     for (n in c(3, -3)) {
         h <- head(sealevel, n)
         look <- head(seq_along(sealevel[["time"]]), n)
         expect_equal(h[["time"]], sealevel[["time"]][look])
         expect_equal(h[["elevation"]], sealevel[["elevation"]][look])
     }
})

 test_that("head_section", {
     data(section)
     for (n in c(-10, 10)) {
         h <- head(section, n)
         expect_equal(h@metadata$stationId, head(section@metadata$stationId, n))
         expect_equal(h@metadata$longitude, head(section@metadata$longitude, n))
         expect_equal(h@metadata$latitude, head(section@metadata$latitude, n))
         expect_equal(h@metadata$time, head(section@metadata$time, n))
         expect_equal(h@data$station, head(section@data$station, n))
     }
})

 if (requireNamespace("ocedata", quietly=TRUE)) {
     test_that("oceApprox", {
         # Test for same values after rewriting the C code in C++.
         d <- data.frame(x=seq(0, 1, length.out=20), y=seq(0, 100, length.out=20))
         da <- oceApprox(d$x, d$y, c(0.4, 0.5, 0.6))
         expect_equal(da, c(40, 50, 60))
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
})}

 test_that("tail_adp", {
     data(adp)
     for (n in c(3, -3)) {
         t <- head(adp, n)
         look <- head(seq_len(dim(adp[["v"]])[1]), n)
         expect_equal(t[["time"]], adp[["time"]][look])
         expect_equal(t[["v"]], adp[["v"]][look,,])
     }
})

 test_that("tail_adv", {
     data(adv)
     for (n in c(3, -3)) {
         h <- head(adv, n)
         look <- head(seq_len(dim(adv[["v"]])[1]), n)
         expect_equal(h[["time"]], adv[["time"]][look])
         expect_equal(h[["v"]], adv[["v"]][look,])
     }
})

 test_that("tail_argo", {
     data(argo)
     ## This test is hard to read, because it is exhaustive, and the data
     ## format for argo objects is tricky, with vectors, matrices, time
     ## vectors that R does not regard as vectors (why?) and character
     ## strings.
     for (n in c(3, -3)) {
         t <- tail(argo, n)
         for (name in names(argo@metadata)) {
             if (name %in% c("direction", "juldQc", "positionQc")) {
                 ## select characters in a string
                 j <- tail(seq_len(nchar(argo@metadata[[name]])), n)
                 expect_equal(t@metadata[[name]],
                     substr(argo@metadata[[name]], j[1], tail(j, 1)))
             } else if (name == "flags") {
                 j <- tail(seq_len(dim(argo@metadata$flags[[1]])[2]), n)
                 for (fname in names(argo@metadata$flags)) {
                     expect_equal(t@metadata$flags[[fname]], argo@metadata$flags[[fname]][, j])
                 }
             } else if (is.vector(argo@metadata[[name]])) {
                 expect_equal(t@metadata[[name]], tail(argo@metadata[[name]], n))
             } else if (is.matrix(argo@metadata[[name]])) {
                 j <- tail(seq_len(dim(argo@metadata[[name]])[2]), n)
                 expect_equal(t@metadata[[name]], argo@metadata[[name]][, j])
             }
         }
         for (name in names(argo@data)) {
             if (is.vector(argo@data[[name]])) {
                 expect_equal(t@data[[name]], tail(argo@data[[name]], n))
             } else if (is.matrix(argo@data[[name]])) {
                 j <- tail(seq_len(dim(argo@data[[name]])[2]), n)
                 expect_equal(t@data[[name]], argo@data[[name]][, j])
             } else if (name == "time") {
                 ## for reasons unknown, time is not a vector
                 expect_equal(t@data[[name]], tail(argo@data[[name]], n))
             } else {
                 warning("ignoring data item: '", name, "'")
             }
         }
     }
})

 test_that("tail_cm", {
     data(cm)
     for (n in c(3, -3)) {
         t <- tail(cm, n)
         look <- tail(seq_along(cm[["time"]]), n)
         for (n in names(cm@data)) {
             expect_equal(t[[n]], cm[[n]][look])
         }
     }
})

 test_that("tail_coastline", {
     data(coastlineWorld)
     for (n in c(-3, 3)) {
         t <- tail(coastlineWorld, n)
         expect_equal(t[["longitude"]], tail(coastlineWorld[["longitude"]], n))
         expect_equal(t[["latitude"]], tail(coastlineWorld[["latitude"]], n))
     }
})

 test_that("tail_ctd", {
     data(section)
     ctd <- section[["station", 100]]
     for (n in c(-10, 10)) {
         t <- tail(ctd, n)
         for (name in names(ctd@data)) {
             expect_equal(t@data[[name]], tail(ctd@data[[name]], n))
             expect_equal(t@metadata$flags[[name]], tail(ctd@metadata$flags[[name]], n))
         }
     }
})

 test_that("tail_echosounder", {
     data(echosounder)
     for (n in c(-10, 10)) {
         t <- tail(echosounder, n=n)
         look <- tail(seq_len(dim(echosounder[["a"]])[1]), n)
         expect_equal(t[["depth"]], echosounder[["depth"]])
         expect_equal(t[["latitude"]], echosounder[["latitude"]][look])
         expect_equal(t[["longitude"]], echosounder[["longitude"]][look])
         expect_equal(t[["time"]], echosounder[["time"]][look])
         expect_equal(t[["a"]], echosounder[["a"]][look, ])
     }
})

 test_that("tail_ladcp", {
     dat <- read.table(text=ladpText, skip=7, header=FALSE, col.names=c("z","u","v","ev"))
     ladp <- as.ladp(longitude=48+58.4580/60, latitude=-(45+52.3932/60),
         station=25, time=as.POSIXct("2014/ 7/ 6",tz="UTC"),
         pressure=dat$z, u=dat$u, v=dat$v)
     for (n in c(3, -3)) {
         t <- tail(ladp, n)
         look <- tail(seq_along(ladp[["pressure"]]), n)
         for (n in names(ladp@data)) {
             expect_equal(t[[n]], ladp[[n]][look])
         }
     }
})

 test_that("tail_lobo", {
     data(lobo)
     for (n in c(3, -3)) {
         t <- tail(lobo, n)
         look <- tail(seq_along(lobo[["time"]]), n)
         for (n in names(lobo@data)) {
             expect_equal(t[[n]], lobo[[n]][look])
         }
     }
})

 test_that("tail_rsk", {
     data(rsk)
     for (n in c(3, -3)) {
         t <- tail(rsk, n)
         look <- tail(seq_along(rsk[["time"]]), n)
         expect_equal(t[["time"]], rsk[["time"]][look])
         expect_equal(t[["elevation"]], rsk[["elevation"]][look])
     }
})

 test_that("tail_sealevel", {
     data(sealevel)
     for (n in c(3, -3)) {
         t <- tail(sealevel, n)
         look <- tail(seq_along(sealevel[["time"]]), n)
         expect_equal(t[["time"]], sealevel[["time"]][look])
         expect_equal(t[["elevation"]], sealevel[["elevation"]][look])
     }
})

 test_that("tail_section", {
     data(section)
     for (n in c(-10, 10)) {
         t <- tail(section, n)
         expect_equal(t@metadata$stationId, tail(section@metadata$stationId, n))
         expect_equal(t@metadata$longitude, tail(section@metadata$longitude, n))
         expect_equal(t@metadata$latitude, tail(section@metadata$latitude, n))
         expect_equal(t@metadata$time, tail(section@metadata$time, n))
         expect_equal(t@data$station, tail(section@data$station, n))
     }
})

 test_that("trim_ts", {
     x <- seq(0, 10, 0.1)
     xlim <- c(2.0, 2.9)
     oce:::trim_ts(x, xlim, 0)
     expect_equal(oce:::trim_ts(x, xlim, 0), list(from=20, to=31))
})

 test_that("concatenate adp", {
     data(adp)
     t0 <- median(adp[["time"]])
     a <- subset(adp, time <= t0)
     b <- subset(adp, time > t0)
     ab <- concatenate(a, b)
     for (n in c("time", "v", "a", "distance"))
         expect_equal(ab[[n]], adp[[n]])
})

 test_that("concatenate adv", {
     data(adv)
     t0 <- median(adv[["time"]])
     a <- subset(adv, time <= t0)
     b <- subset(adv, time > t0)
     ab <- concatenate(a, b)
     for (n in c("time", "v", "a", "distance"))
         expect_equal(ab[[n]], adv[[n]])
})

 test_that("concatenate ctd", {
     data(ctd)
     scan0 <- median(ctd[["scan"]])
     a <- subset(ctd, scan <= scan0)
     b <- subset(ctd, scan > scan0)
     ab <- concatenate(a, b)
     for (n in c("scan", "pressure", "salinity", "temperature"))
         expect_equal(ab[[n]], ctd[[n]])
})

 test_that("times", {
     jd <- julianDay(as.POSIXct("2018-07-01 12:00:00", tz="UTC"))
     t <- numberAsPOSIXct(cbind(jd, 1e3 * 1 * 3600), type="epic", tz="UTC")
     expect_equal(t, as.POSIXct("2018-07-01 01:00:00", tz="UTC"))
})

