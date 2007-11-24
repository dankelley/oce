# Test of handling CTD data
library(oce)
profile <- oce::read.oce("ctd01.cnv",debug=TRUE)
summary(profile)
