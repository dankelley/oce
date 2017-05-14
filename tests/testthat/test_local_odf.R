## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## INSTRUCTIONS: Any time a new file is added to local_data, add
## some tests here.

## FIXME: test units here also, for at least one case.

library(oce)
context("files ODF format")
test_that("Bedford Basin CTD profiles", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              d <- read.oce("local_data/bedford_basin/CTD_BCD2010667_001_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2010-01-05 13:23:29", tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenCurrent="OCUR_01", oxygenTemperature="OTMP_01", unknown="UNKN_01",
                                    fluorometer="FLOR_01", par="PSAR_01", salinity="PSAL_01", oxygen="DOXY_01",
                                    sigmaTheta="SIGP_01", flagArchaic="FFFF_01"))

              d <- read.oce("local_data/bedford_basin/CTD_BCD2011667_001_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2011-01-05 09:10:36", tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenCurrent="OCUR_01", oxygenTemperature="OTMP_01", unknown="UNKN_01",
                                    fluorometer="FLOR_01", par="PSAR_01", salinity="PSAL_01", oxygen="DOXY_01",
                                    sigmaTheta="SIGP_01", flagArchaic="FFFF_01"))

              d <- read.oce("local_data/bedford_basin/CTD_BCD2012667_01_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2012-01-04 12:53:38", tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "oxygenSaturation", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenCurrent="OCUR_01", oxygenTemperature="OTMP_01", unknown="UNKN_01",
                                    fluorometer="FLOR_01", par="PSAR_01", salinity="PSAL_01", oxygen="DOXY_01",
                                    sigmaTheta="SIGP_01", oxygenSaturation="OSAT_01", flagArchaic="FFFF_01"))

              d <- read.oce("local_data/bedford_basin/CTD_BCD2013667_001_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2013-01-02 15:04:39", tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenCurrent="OCUR_01", oxygenTemperature="OTMP_01", unknown="UNKN_01",
                                    fluorometer="FLOR_01", par="PSAR_01", salinity="PSAL_01", oxygen="DOXY_01",
                                    sigmaTheta="SIGP_01", flagArchaic="FFFF_01"))

              expect_warning(d <- read.oce("local_data/bedford_basin/CTD_BCD2014667_001_01_DN.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_equal(d[['startTime']], as.POSIXct("2014-01-08 13:37:15", tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenVoltage", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenVoltage="OXYV_01", unknown="UNKN_01",
                                    fluorometer="FLOR_01", par="PSAR_01", salinity="PSAL_01", oxygen="DOXY_01",
                                    sigmaTheta="SIGP_01", flagArchaic="FFFF_01"))
              expect_warning(d <- read.oce("local_data/bedford_basin/CTD_BCD2015667_001_01_DN.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_equal(d[['startTime']], as.POSIXct("2015-01-07 13:28:34", tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenVoltage", "fluorometer",
                             "par", "salinity", "oxygen", "sigmaTheta", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenVoltage="OXYV_01", fluorometer="FLOR_01", par="PSAR_01",
                                    salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01", flagArchaic="FFFF_01"))
              expect_warning(d <- read.oce("local_data/bedford_basin/D16667001.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_equal(d[['startTime']], as.POSIXct("2016-01-06 13:17:37",tz="UTC"))
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenVoltage", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "flagArchaic"))
              expect_identical(d@metadata$dataNamesOriginal,
                               list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01", conductivity="CRAT_01",
                                    oxygenVoltage="OXYV_01", unknown="UNKN", fluorometer="FLOR_01", par="PSAR_01",
                                    salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01", flagArchaic="FFFF_01"))


          }
})

