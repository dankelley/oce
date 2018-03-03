## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
context("General tests")

test_that("binCount1D", {
          bc1 <- binCount1D(1:100,seq(0,100,10))
          expect_equal(bc1$xbreaks, seq(0, 100,10))
          expect_equal(bc1$xmids, seq(5, 95, 10))
          expect_equal(bc1$number, rep(10, 10))
          ## following results checked by eye
          set.seed(123)
          x <- rnorm(10)
          y <- rnorm(10)
          bc2 <- binCount2D(x, y, seq(-2,2,1), seq(-2,2,1))
          expect_equal(bc2$xbreaks, c(-2, -1, 0, 1, 2))
          expect_equal(bc2$ybreaks, c(-2, -1, 0, 1, 2))
          expect_equal(bc2$xmids, c(-1.5, -0.5, 0.5, 1.5))
          expect_equal(bc2$ymids, c(-1.5, -0.5, 0.5, 1.5))
          expect_equal(bc2$number,
                       rbind(c(1, 0, 0, 0),
                             c(0, 1, 2, 1),
                             c(0, 1, 2, 0),
                             c(0, 0, 1, 1)))
})

test_that("binCount2D", {
          bc <- binCount1D(1:100,seq(0,100,10))
          expect_equal(bc$xbreaks, seq(0, 100,10))
          expect_equal(bc$xmids, seq(5, 95, 10))
          expect_equal(bc$number, rep(10, 10))
})


test_that("oceEdit", {
          data(ctd)
          ## metadata
          a <- oceEdit(ctd, "waterDepth", 100)
          expect_equal(a@metadata$waterDepth, 100)
          b <- oceEdit(a, "waterDepth", 200)
          expect_equal(b@metadata$waterDepth, 200)
          c <- oceEdit(b, "metadata@waterDepth", 300)
          expect_equal(c@metadata$waterDepth, 300)
          ## data
          scan <- ctd[["scan"]]
          ctd2 <- oceEdit(ctd, "scan", scan+1)
          expect_true("scan" %in% names(ctd2[["data"]]))
          expect_false("scan" %in% names(ctd2[["metadata"]]))
          expect_equal(ctd[["scan"]]+1, ctd2[["scan"]])
          ## check "case 1A" of docs (cannot add non-extant item)
          expect_error(oceEdit(ctd, "noSuchThing", 1), "nothing named 'noSuchThing'")
          ## check "case 2" of docs (can add non-extant item, if slot provided)
          ctd2 <- oceEdit(ctd, "metadata@newMetadata", 1)
          expect_true("newMetadata" %in% names(ctd2[["metadata"]]))
          ctd2 <- oceEdit(ctd, "data@newData", 1)
          expect_true("newData" %in% names(ctd2[["data"]]))
})


test_that("times", {
          expect_equal(numberAsPOSIXct(719529, "matlab"), ISOdatetime(1970,1,1,0,0,0,tz="UTC"))
          ## The GPS test value was calculated as follows:
          ## https://www.labsat.co.uk/index.php/en/gps-time-calculator
          ## gives week=604 and sec=134336 (for the indicated date), IGNORING
          ## leap seconds. However,
          ## https://confluence.qps.nl/display/KBE/UTC+to+GPS+Time+Correction#UTCtoGPSTimeCorrection-UTC(CoordinatedUniversalTime)
          ## indicates that a 15-second correction was needed for GPS to UTC, so
          ## we do that in the test value.
          expect_equal(numberAsPOSIXct(cbind(604, 134336+15), type="gps"),
                       as.POSIXct("2011-03-21 13:18:56",tz="UTC"))
          ## Matlab times; see http://www.mathworks.com/help/matlab/ref/datenum.html
          mt <- 7.362007209411687e5
          expect_equal(as.numeric(numberAsPOSIXct(mt, "matlab", tz="UTC")),
                       as.numeric(as.POSIXct("2015-08-24 17:18:09", tz="UTC")), tolerance=1)
          ## NCEP1 times; test value from
          ## http://coastwatch.pfeg.noaa.gov/erddap/convert/time.html?isoTime=2015-09-04T12%3A00%3A00Z&units=hours+since+1800-01-01
          expect_equal(as.numeric(numberAsPOSIXct(1890564, "ncep1")),
                       as.numeric(as.POSIXct("2015-09-04 12:00:00", tz="UTC")), tolerance=1)
          ## NCEP2 times; see http://www.esrl.noaa.gov/psd/data/gridded/faq.html#3
          ## and also https://github.com/dankelley/oce/issues/739, the latter
          ## documenting what is essentially a kludge for this to work.
          expect_equal(as.numeric(numberAsPOSIXct(725738, "ncep2")),
                       as.numeric(as.POSIXct("1988-01-01 00:00:00", tz="UTC")), tolerance=1)
})

test_that("integrateTrapezoid", {
          x <- seq(0, 1, length.out=10)
          y <- rep(1, length(x))
          expect_equal(1, integrateTrapezoid(x, y))
          expect_equal(4, integrateTrapezoid(x, y, xmin=-2, xmax=2))
          expect_equal(9, integrateTrapezoid(rep(1, 10)))
          x <- seq(0, 1, length.out=10)
          y <- 2*x + 3*x^2
          expect_equal(2, integrateTrapezoid(x, y), tolerance=0.01)
          expect_equal(integrateTrapezoid(x,y), integrateTrapezoid(y) * (x[2]-x[1]))
          expect_equal(c(0.0000000000000, 0.0144032921811, 0.0473251028807,
                         0.0884773662551, 0.1378600823045, 0.1954732510288,
                         0.2613168724280, 0.3353909465021, 0.4176954732510,
                         0.5082304526749),
                       integrateTrapezoid(x, y, "dA"))
          expect_equal(c(0.0000000000000, 0.0144032921811, 0.0617283950617,
                         0.1502057613169, 0.2880658436214, 0.4835390946502,
                         0.7448559670782, 1.0802469135802, 1.4979423868313,
                         2.0061728395062),
                       integrateTrapezoid(x, y, "cA"))
})

test_that("matchBytes", {
          buf <- as.raw(c(0xa5, 0x11, 0xaa, 0xa5, 0x11, 0x00))
          expect_equal(c(1,4), matchBytes(buf, 0xa5, 0x11))
})

test_that("time-series filtering", {
          b <- rep(1,5)/5
          a <- 1
          x <- seq(1, 4, by=0.2)
          matlab.res <- c(0.2000,0.4400,0.7200,1.0400,1.4000,1.6000,1.8000,2.0000,2.2000,
                          2.4000,2.6000,2.8000,3.0000,3.2000,3.4000,3.6000)
          expect_equal(matlab.res, oce.filter(x, a, b))
})

test_that("Magnetic field at Halifax", {
          ## test values from http://www.geomag.bgs.ac.uk/data_service/models_compass/wmm_calc.html
          expect_equal(-17.976, magneticField(-63.562,44.640,2013)$declination,tolerance=1e-3)
          expect_equal(67.562, magneticField(-63.562,44.640,2013)$inclination,tolerance=1e-3)
          expect_equal(52096, magneticField(-63.562,44.640,2013)$intensity,tolerance=1e-3)
})

test_that("Coriolis", {
          f <- coriolis(45)
          expect_equal(f, 1.031261e-4, tolerance=1e-6)
})

test_that("Gravity", {
          g <- gravity(45)
          expect_equal(g, 9.8, tolerance=1e-2)
})


test_that("despike", { # issue 1067
          min <- 10000
          max <- 20000
          x1 <- c(3715, 7546, 10903, 13386, 15196, 15371, 55748, 71488)
          x2 <- despike(x1, reference="trim", min=min, max=max, replace="reference")
          x3 <- x1
          x3[1:2] <- 10903 # result from approx() with rule=2
          x3[7:8] <- 15371 # result from approx() with rule=2
          expect_equal(x2, x3)
          x4 <- despike(x1, reference="trim",min=min,max=max, replace="NA")
          x5 <- x1
          x5[x5<min] <- NA
          x5[x5>max] <- NA
          expect_equal(x4, x5)
})


test_that("integration", {
          x <- seq(0, 1, length.out=10)
          dx <- x[2] - x[1]
          y <- 2*x + 3*x^2
          A <- integrateTrapezoid(x, y)
          expect_equal(A,2,tolerance=dx^2) # test for quadratic accuracy
})

test_that("interpBarnes", {
          data(wind)
          u <- interpBarnes(wind$x, wind$y, wind$z)
          ## These tests are not in comparison to theory, or
          ## known values; they simply ensure that results have not
          ## changed since 2016-11-06, when the tests were devised.
          expect_equal(u$zg[1,1], 30.962611975027)
          expect_equal(u$zg[10,10], 27.042654784966)
})

test_that("binAverage", {
          x <- seq(0, 100, 1)
          y <- 1 + x ^2
          ba <- binAverage(x, y)
          ## These tests are not in comparison to theory, or
          ## known values; they simply ensure that results have not
          ## changed since 2016-11-06, when the tests were devised.
          expect_equal(10, length(ba$x))
          expect_equal(10, length(ba$y))
          expect_equal(ba$x[5], 45)
          expect_equal(ba$y[5], 1989.5)
})

test_that("binApply1D simple", {
          set.seed(123)
          n <- 3
          x <- runif(n)
          f <- x^2
          b <- binApply1D(x, f, xbreaks=seq(0,1,0.25), FUN=mean)
          expect_equal(b$xbreaks, c(0, 0.25, 0.5, 0.75, 1))
          expect_equal(b$xmids, c(0.125, 0.375, 0.625, 0.875))
          expect_equal(b$result, c(NA, 0.1249814763, 0.6214249866))
})

test_that("binApply1D with missing bins", {
          ## 'result' should have no NA values
          x <- seq(0, 95)
          xbreaks <- seq(0, 100, 10)
          y <- x
          b <- binApply1D(x, y, xbreaks, mean)
          expect_equal(length(b$xmids), length(b$result))
          ## should be one NA at the end of 'result'
          x <- seq(0, 89)
          xbreaks <- seq(0, 100, 10)
          y <- x
          b <- binApply1D(x, y, xbreaks, mean)
          expect_equal(length(b$xmids), length(b$result))
          expect_true(is.na(tail(b$result, 1)))
          ## should be an NA at both start and end of 'result'
          x <- seq(11, 89)
          xbreaks <- seq(0, 100, 10)
          y <- x
          b <- binApply1D(x, y, xbreaks, mean)
          expect_equal(length(b$xmids), length(b$result))
          expect_true(is.na(b$result[1]))
          expect_true(is.na(tail(b$result, 1)))
})

test_that("binApply2D", {
          set.seed(123)
          n <- 10
          x <- runif(n)
          y <- runif(n)
          z <- outer(x, y)
          b <- binApply2D(x, y, z, xbreaks=seq(0,1,0.25), ybreaks=seq(0,1,0.25), FUN=mean)
          expect_equal(names(b), c("xbreaks", "xmids", "ybreaks", "ymids", "result"))
          expect_equal(b$result[1,], c(0.27639419054, 0.28860414805, 0.21404595826, 0.02384287241))
          expect_equal(b$result[2,], c(0.4796382017, 0.4125746934, 0.4621441855, 0.1947436835))
          expect_equal(b$result[3,], c(0.27639419054, 0.28860414805, 0.21404595826, 0.02384287241))
})

test_that("get_bit (unused in oce)", {
          buf <- 0x3a
          bits <- unlist(lapply(7:0, function(i) .Call("get_bit", buf, i)))
          ## NB. 'i' starts at rightmost bit
          expect_equal(c(0, 0, 1, 1, 1, 0, 1, 0), bits)
})

