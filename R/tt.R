library(oce)
f <- "CTD_BCD2010666_01_01_DN.ODF"
source('~/src/R-kelley/oce/R/oce.R')
source('~/src/R-kelley/oce/R/ctd.R')
d <- read.oce(f)
plot(d)
##plot(as.ctd(d$PSAL_01, d$TEMP_01, d$PRES_01))
