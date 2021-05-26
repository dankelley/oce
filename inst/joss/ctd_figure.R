png("ctd_figure.png", width=7, height=6, unit="in", res=300, pointsize=12)

library(oce)                           # load library
data(ctd)                              # sea level in Halifax Harbour
plot(ctd)                              # plot summary diagram

dev.off()

