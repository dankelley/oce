png("figure_1.png", unit="in", width=7, height=7, res=300, pointsize=11)

library(oce)
data(ctd)
plot(ctd)

dev.off()

