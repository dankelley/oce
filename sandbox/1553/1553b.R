## Produce very detailed output for visual comparison with ODF

library(oce)
##> source("~/git/oce/R/odf.R")
d <- read.odf("CTD_DGR2005-001_9_2_DN.ODF", debug=4)
sink("test_list.txt")
str(d[["header"]], list.len=5000)
sink()



