library(oce)
context("lobo")
test_that("lobo", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              lobo <- read.lobo("local_data/lobo.dat")
              expect_equal(sort(names(lobo[["metadata"]])), c("filename", "flags", "units"))
              expect_equal(sort(names(lobo[["data"]])),
                           c("airtemperature", "fluorescence", "nitrate", "pressure", "salinity",
                             "temperature", "time", "u", "v")) 
          }
})

