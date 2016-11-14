library(oce)
context("coastline")
test_that("coastline shapefile", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              cl <- read.oce("local_data/ne_110m_admin_0_countries.shp")
              expect_equal(length(cl[['longitude']]), 10696)
              expect_equal(head(cl[['longitude']]), c(NA,61.2108170917257,62.2306514830059,62.9846623065766,63.1935384459004,63.98289594915870))
              expect_equal(head(cl[['latitude']]), c(NA,35.6500723333092,35.2706639674223,35.4040408391676,35.8571656357189,36.0079574651466))

          }
})

