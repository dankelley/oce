## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## INSTRUCTIONS: Any time a new file is added to local_data, add
## some tests here.

library(oce)

context("ODF files")

test_that("Flemish Cap adcp file (with malformed CODE tokens that lack ' characters)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              d <- read.oce("local_data/flemish_cap/MADCPS_hud2013021_1840_12556-106_3600.ODF")
              ## oce names
              expect_equal(names(d[["data"]]), c("u", "v", "w", "error", "a", "unknown", "time"))
              ## original names
              orig <-  list(u="EWCT_01", v="NSCT_01", w="VCSP_01", error="ERRV_01",
                            a="BEAM_01", unknown="UNKN_01", time="SYTM_01")
              expect_equal(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["uUnit"]]$unit, expression(m/s))
              expect_equal(d[["vUnit"]]$unit, expression(m/s))
              expect_equal(d[["wUnit"]]$unit, expression(m/s))
              expect_equal(d[["errorUnit"]]$unit, expression(m/s))
           }
})

test_that("Flemish Cap microcat file (with malformed CODE tokens that lack ' characters)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              d <- read.oce("local_data/flemish_cap/MCM_HUD2013021_1840_2305_300.ODF")
              ## oce names
              expect_equal(names(d[["data"]]), c("temperature", "pressure", "time", "conductivity", "salinity", "theta", "sigmaTheta"))
              ## original names
              orig <- list(temperature="TEMP_01", pressure="PRES_01",
                           time="SYTM_01", conductivity="CRAT_01",
                           salinity="PSAL_01", theta="POTM_01", sigmaTheta="SIGP_01")
              expect_equal(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["conductivityUnit"]]$unit, expression())
              expect_equal(d[["salinityUnit"]]$unit, expression())
              expect_equal(d[["thetaUnit"]]$unit, expression(degree*C))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
          }
})

test_that("Bedford Basin CTD profile 1 (with proper CODE tokens)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              d <- read.oce("local_data/bedford_basin/CTD_BCD2010667_001_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2010-01-05 13:23:29", tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenCurrent="OCUR_01",
                           oxygenTemperature="OTMP_01", unknown="UNKN_01",
                           fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01")
              expect_equal(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["conductivityUnit"]]$unit, expression())
              expect_equal(d[["oxygenCurrentUnit"]]$unit, expression(mu*a))
              expect_equal(d[["oxygenTemperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              expect_equal(d[["parUnit"]]$unit, expression(mu*einstein/s/m^2))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
          }
})

test_that("Bedford Basin CTD profile 2 (with proper CODE tokens)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              d <- read.oce("local_data/bedford_basin/CTD_BCD2011667_001_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2011-01-05 09:10:36", tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenCurrent="OCUR_01",
                           oxygenTemperature="OTMP_01", unknown="UNKN_01",
                           fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01")
              expect_identical(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["conductivityUnit"]]$unit, expression())
              expect_equal(d[["oxygenCurrentUnit"]]$unit, expression(mu*a))
              expect_equal(d[["oxygenTemperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              expect_equal(d[["parUnit"]]$unit, expression(mu*einstein/s/m^2))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
           }
})

test_that("Bedford Basin CTD profile 3 (with proper CODE tokens)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              d <- read.oce("local_data/bedford_basin/CTD_BCD2012667_01_01_DN.ODF")
              expect_equal(d[['startTime']], as.POSIXct("2012-01-04 12:53:38", tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta", "oxygenSaturation"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenCurrent="OCUR_01",
                           oxygenTemperature="OTMP_01", unknown="UNKN_01",
                           fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01",
                           oxygenSaturation="OSAT_01")
              expect_identical(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["conductivityUnit"]]$unit, expression())
              expect_equal(d[["oxygenCurrentUnit"]]$unit, expression(mu*a))
              expect_equal(d[["oxygenTemperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              expect_equal(d[["parUnit"]]$unit, expression(mu*einstein/s/m^2))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
           }
})

test_that("Bedford Basin CTD profile 4 (with proper CODE tokens but no units for OCUR_01, OTMP_01, CRAT_01, FLOR_01, PSAR_01, PSAL_01,
          DOXY_01, or SIGP_01)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              expect_warning(d <- read.oce("local_data/bedford_basin/CTD_BCD2013667_001_01_DN.ODF"),
                             "source file does not indicate a unit for pressure \\(and perhaps for other items\\)")
              expect_equal(d[['startTime']], as.POSIXct("2013-01-02 15:04:39", tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenCurrent", "oxygenTemperature", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenCurrent="OCUR_01",
                           oxygenTemperature="OTMP_01", unknown="UNKN_01",
                           fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01")
              expect_identical(d[["dataNamesOriginal"]], orig)
              ## units -- note that this is a weird file, which lacks units.
              expect_equal(d[["pressureUnit"]]$unit, expression())
              expect_equal(d[["temperatureUnit"]]$unit, expression())
              ## expect_equal(d[["conductivityUnit"]]$unit, expression(S/m))
              ## expect_equal(d[["oxygenCurrentUnit"]]$unit, expression(mu*a))
              ## expect_equal(d[["oxygenTemperatureUnit"]]$unit, expression(degree*C))
              ## expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              ## expect_equal(d[["parUnit"]]$unit, expression(mu*einstein/s/m^2))
              ## expect_equal(d[["oxygenUnit"]]$unit, expression(kg/m^3))
              ## expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
           }
})

test_that("Bedford Basin CTD profile 5 (with proper CODE tokens but no unit for PSAR_01)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              expect_warning(d <- read.oce("local_data/bedford_basin/CTD_BCD2014667_001_01_DN.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_equal(d[['startTime']], as.POSIXct("2014-01-08 13:37:15", tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenVoltage", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenVoltage="OXYV_01",
                           unknown="UNKN_01", fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01")
              expect_identical(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["conductivityUnit"]]$unit, expression(S/m))
              expect_equal(d[["oxygenVoltageUnit"]]$unit, expression(V))
              expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              ## expect_equal(d[["parUnit"]]$unit, expression(mu*einstein/s/m^2))
              expect_equal(d[["oxygenUnit"]]$unit, expression(ml/l))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
           }
})

test_that("Bedford Basin CTD profile 6 (with proper CODE tokens)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              expect_warning(d <- read.oce("local_data/bedford_basin/CTD_BCD2015667_001_01_DN.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_equal(d[['startTime']], as.POSIXct("2015-01-07 13:28:34", tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenVoltage", "fluorometer",
                             "par", "salinity", "oxygen", "sigmaTheta"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenVoltage="OXYV_01",
                           fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01")
              expect_identical(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["conductivityUnit"]]$unit, expression(S/m))
              expect_equal(d[["oxygenVoltageUnit"]]$unit, expression(V))
              expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              expect_equal(d[["parUnit"]]$unit, expression())
              expect_equal(d[["salinityUnit"]]$unit, expression())
              expect_equal(d[["oxygenUnit"]]$unit, expression(ml/l))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
           }
})

test_that("Bedford Basin CTD profile 7 (with proper CODE tokens but no PSAR_01 unit)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              expect_warning(d <- read.oce("local_data/bedford_basin/D16667001.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_warning(d <- read.oce("local_data/bedford_basin/D16667001.ODF"),
                             "\"CRAT_01\" should be unitless")
              expect_equal(d[['startTime']], as.POSIXct("2016-01-06 13:17:37",tz="UTC"))
              ## oce names
              expect_equal(names(d[["data"]]),
                           c("scan", "pressure", "temperature", "conductivity", "oxygenVoltage", "unknown",
                             "fluorometer", "par", "salinity", "oxygen", "sigmaTheta"))
              ## original names
              orig <- list(scan="CNTR_01", pressure="PRES_01", temperature="TEMP_01",
                           conductivity="CRAT_01", oxygenVoltage="OXYV_01",
                           unknown="UNKN", fluorometer="FLOR_01", par="PSAR_01",
                           salinity="PSAL_01", oxygen="DOXY_01", sigmaTheta="SIGP_01")
              expect_identical(d[["dataNamesOriginal"]], orig)
              ## units
              expect_equal(d[["pressureUnit"]]$unit, expression(dbar))
              expect_equal(d[["temperatureUnit"]]$unit, expression(degree*C))
              expect_equal(d[["conductivityUnit"]]$unit, expression(S/m))
              expect_equal(d[["oxygenVoltageUnit"]]$unit, expression(V))
              expect_equal(d[["fluorometerUnit"]]$unit, expression(mg/m^3))
              ## expect_equal(d[["parUnit"]]$unit, expression(mu*einstein/s/m^2))
              expect_equal(d[["salinityUnit"]]$unit, expression())
              expect_equal(d[["oxygenUnit"]]$unit, expression(ml/l))
              expect_equal(d[["sigmaThetaUnit"]]$unit, expression(kg/m^3))
          }
})

