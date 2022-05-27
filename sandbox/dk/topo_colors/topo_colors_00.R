## NOTE: you must run this from oce/sandbox/dk/topo_colours directory for the
## source() to work

library(oce)

png("topo_colors_00_A.png")
cm <- colormap(c(-100, 100),
               col=function(n) oceColorsGebco(region="both", n=n))
drawPalette(colormap=cm, pos=3)
dev.off()

png("topo_colors_00_B.png")
##source("../../../R/oce.R")
##source("../../../R/colors.R")
cm <- colormap(c(-100, 100),
               col=function(n) oceColorsGebco(region="both", n=n))
drawPalette(colormap=cm, pos=3)
dev.off()

