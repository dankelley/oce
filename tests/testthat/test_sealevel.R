test_that("Can read type 1 data",
          {
              data <- paste("Station_Name,HALIFAX",
                            "Station_Number,490",
                            "Latitude_Decimal_Degrees,44.666667",
                            "Longitude_Decimal_Degrees,63.583333",
                            "Datum,CD",
                            "Time_zone,UTC",
                            "SLEV=Observed Water Level",
                            "Obs_date,SLEV(metres)",
                            "2003/01/01 05:00,0.57,",
                            "2003/01/01 06:00,0.63,",
                            "2003/01/01 07:00,1.12,", sep="\n")
              file <- textConnection(data)
              sealevel <- read.sealevel(file)
              t <- as.POSIXct(c("2003/01/01 05:00", "2003/01/01 06:00", "2003/01/01 07:00"), tz="UTC")
              expect_equal(sealevel[["time"]], t)
              expect_equal(sealevel[["elevation"]], c(0.57, 0.63, 1.12))
              close(file)
          }
)

test_that("Can read type 1 data",
          {
              data <- c("Station_Name,HALIFAX",
                        "Station_Number,490",
                        "Latitude_Decimal_Degrees,44.666667",
                        "Longitude_Decimal_Degrees,63.583333",
                        "Datum,CD",
                        "Time_zone,UTC",
                        "SLEV=Observed Water Level",
                        "Obs_date,SLEV(metres)",
                        "2003/01/01 05:00,0.57,",
                        "2003/01/01 06:00,0.63,",
                        "2003/01/01 07:00,1.12,",
                        "2003/01/01 08:00,1.54,",
                        "2003/01/01 09:00,1.97,",
                        "2003/01/01 10:00,2,", sep="\n")
              file <- textConnection(data)
              sealevel <- read.sealevel(file)
              t <- seq(as.POSIXct("2003/01/01 05:00", tz="UTC"), by="1 hour", length.out=6)
              expect_equal(sealevel[["time"]], t)
              expect_equal(sealevel[["elevation"]], c(0.57, 0.63, 1.12, 1.54, 1.97, 2))
              close(file)
          }
)

test_that("Can read type 3 data",
          {
              data <- paste("275HALIFAX 2019  LAT=44 40.0N  LONG=063 35.0W  TIMEZONE=GMT",
                            "275HALIFAX 2019 1 11  930  681  647  577  806 1115 1498 1747 1971 1828 1723 1396",
                            "275HALIFAX 2019 1 12  930  681  647  577  806 1115 1498 1747 1971 1828 1723 1396",
                            "275HALIFAX 2019 1101 1390 1777 1999 2160 1793 1628 1259 1227 1057 1142 1032 1288",
                            "275HALIFAX 2019 1102 1390 1777 1999 2160 1793 1628 1259 1227 1057 1142 1032 1288",
                            "275HALIFAX 201910101 1390 1777 1999 2160 1793 1628 1259 1227 1057 1142 1032 1288",
                            "275HALIFAX 201910102 1390 1777 1999 2160 1793 1628 1259 1227 1057 1142 1032 1288", sep="\n")
              file <- textConnection(data)
              sealevel <- read.sealevel(file)
              elevation <- c(0.930, 0.681, 0.647, 0.577, 0.806, 1.115, 1.498, 1.747, 1.971, 1.828, 1.723, 1.396, 0.930,
                             0.681, 0.647, 0.577, 0.806, 1.115, 1.498, 1.747, 1.971, 1.828, 1.723, 1.396, 1.390, 1.777,
                             1.999, 2.160, 1.793, 1.628, 1.259, 1.227, 1.057, 1.142, 1.032, 1.288, 1.390, 1.777, 1.999,
                             2.160, 1.793, 1.628, 1.259, 1.227, 1.057, 1.142, 1.032, 1.288, 1.390, 1.777, 1.999, 2.160,
                             1.793, 1.628, 1.259, 1.227, 1.057, 1.142, 1.032, 1.288, 1.390, 1.777, 1.999, 2.160, 1.793,
                             1.628, 1.259, 1.227, 1.057, 1.142, 1.032, 1.288)
              expect_equal(elevation, sealevel[["elevation"]])
              expect_equal(seq.POSIXt(as.POSIXct("2019-01-01 00:00:00",tz="UTC"),by="1 hour", length.out=24),
                           sealevel[["time"]][1:24])
              expect_equal(seq.POSIXt(as.POSIXct("2019-01-10 00:00:00",tz="UTC"),by="1 hour", length.out=24),
                           sealevel[["time"]][25:48])
              expect_equal(seq.POSIXt(as.POSIXct("2019-10-10 00:00:00",tz="UTC"),by="1 hour", length.out=24),
                           sealevel[["time"]][49:72])
              close(file)
          }
)


