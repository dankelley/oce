library(oce)
source("~/git/oce/R/oce.R")
source("~/git/oce/R/ctd.sbe.R")
f <- system.file("extdata", "d201211_0011.cnv", package="oce")
#d <- read.ctd.sbe(f, encoding="latin1")
d <- read.oce(f)#, encoding="latin1")
dno <- d[["dataNamesOriginal"]]
a <- dno$sigmaTheta
target <- "sigma-é00"
Encoding(target) <- "unknown"
stopifnot(a == target)
cat("all ok\n")

