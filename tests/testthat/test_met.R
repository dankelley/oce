# vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

library(oce)

csv1 <- system.file("extdata", "test_met_vsn1.csv", package="oce")
csv2 <- system.file("extdata", "test_met_vsn2.csv", package="oce")
xml2 <- system.file("extdata", "test_met_xml2.xml", package="oce")

test_that("oceMagic understands vsn1-style met data files",
          {
              expect_equal("met/csv1", oceMagic(csv1))
          }
)

test_that("oceMagic understands vsn2-style met data files",
          {
              expect_equal("met/csv2", oceMagic(csv2))
          }
)

test_that("oceMagic understands xml2-style met data files",
          {
              expect_equal("met/xml2", oceMagic(xml2))
          }
)

test_that("read.met() handles type=\"csv1\" files",
          {
              d <- read.met(csv1)
              expect_equal(names(d@data), c("dataQuality", "dewPoint",
                                            "direction", "humidex", "humidity",
                                            "pressure", "speed", "temperature",
                                            "time", "u", "v", "visibility",
                                            "weather", "wind", "windChill"))
              expect_equal(d[["latitude"]], 44.88)
              expect_equal(d[["longitude"]], -63.5)
              expect_equal(d[["elevation"]], 145.4)
              expect_equal(d[["station"]], "HALIFAX STANFIELD INT'L A")
              expect_equal(d[["climateIdentifier"]], "8202250")
              expect_equal(d[["WMOIdentifier"]], "71395")
              expect_equal(d[["TCIdentifier"]], "YHZ")
              expect_equal(d[["temperature"]], c(12.1, 11.8, 11.4, 10.9, 10.9))
              expect_equal(d[["humidity"]], c(77, 76, 74, 73, 76))
              expect_equal(d[["wind"]], c(7, 7, 6, 6, 6))
              expect_equal(d[["u"]], c(-6.650391676e-01, -6.650391676e-01, 2.041077999e-16,
                                       1.071312683e+00, 1.276740739e+00))
              expect_equal(d[["v"]], c(1.827180096, 1.827180096, 1.666666667,
                                       1.276740739, 1.071312683))
              expect_equal(d[["time"]], as.POSIXct(c("2003-09-01 00:00:00", "2003-09-01 01:00:00",
                                                     "2003-09-01 02:00:00", "2003-09-01 03:00:00",
                                                     "2003-09-01 04:00:00"), tz="UTC"))
          }
)

test_that("read.oce(file) and read.met(file, type=\"csv1\") give same metadata and data slots",
          {
              doce <- read.oce(csv1)
              dmet <- read.met(csv1)
              expect_equal(doce[["data"]], dmet[["data"]])
              moce <- doce[["metadata"]]
              mmet <- dmet[["metadata"]]
              expect_equal(moce[names(moce)!="filename"], mmet[names(mmet)!="filename"])
          }
)

test_that("read.met() handles type=\"csv2\" files",
          {
              d <- expect_silent(read.met(csv1))
              expect_equal(sort(names(d[["data"]])),
                           c("dataQuality", "dewPoint", "direction", "humidex",
                             "humidity", "pressure", "speed", "temperature",
                             "time", "u", "v", "visibility", "weather", "wind",
                             "windChill"))
          }
)

test_that("read.oce(file) and read.met(file, type=\"csv2\") give same metadata and data slots",
          {
              doce <- read.oce(csv2)
              dmet <- read.met(csv2)
              expect_equal(doce[["data"]], dmet[["data"]])
              moce <- doce[["metadata"]]
              mmet <- dmet[["metadata"]]
              expect_equal(moce[names(moce)!="filename"], mmet[names(mmet)!="filename"])
          }
)

if (requireNamespace("XML", quietly=TRUE))
{
    test_that("read.oce(file) and read.met(file, type=\"xml2\") give same metadata and data slots",
              {
                  doce <- read.oce(xml2)
                  dmet <- read.met(xml2)
                  expect_equal(doce[["data"]], dmet[["data"]])
                  moce <- doce[["metadata"]]
                  mmet <- dmet[["metadata"]]
                  expect_equal(moce[names(moce)!="filename"], mmet[names(mmet)!="filename"])
              }
    )
}
