library(oce)
source("~/git/oce/R/cm.R")
data(adp)
data(adv)
cmAdp <- as.cm(adp)
cmAdv <- as.cm(adv)
plot(cmAdv)
plot(adv)

