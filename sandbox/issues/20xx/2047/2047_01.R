# sandbox/issues/20xx/2049/2049_01.R
#
# auto-create x label for plotProfile() with xtype=vector
#
# https://github.com/dankelley/oce/issues/2047
library(oce)
#if (file.exists("~/git/oce/R/ctd.R")) source("~/git/oce/R/ctd.R")
data(ctd)
for (i in 1:3) {
    if (i == 1) pdf("2047_01.pdf")
    else if (i == 2) png("2047_01_lowres.png")
    else if (i == 3) png("2047_01_hires.png", width=7, height=7, res=250, unit="in")
    par(mfrow=c(1, 3))
    plotProfile(ctd, xtype=ctd[["SA"]]+10)
    plotProfile(ctd, xtype=ctd[["SA"]]+10, xlab="Absolute Salinity plus 10")
    plotProfile(ctd, xtype="SA")
    dev.off()
}
