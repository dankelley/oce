library(oce)

context("met")

d <- read.met("test_met.dat.gz")
expect_equal(names(d@data),c("dataQuality", "temperature", "dewPoint",
                             "humidity", "direction", "wind", "visibility",
                             "pressure", "humidex", "windChill", "weather",
                             "speed", "u", "v", "time"))
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

