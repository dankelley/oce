library(oce)
source("~/git/oce/R/imagep.R")
source("~/git/oce/R/colors.R")
par(mfrow=c(2,2))
z <- seq(-10e3, 10e3, 250)
debug <- 0

cm1 <- colormap(z=z, name="gmt_globe", debug=debug)
par(mar=c(3,3,1,1))
drawPalette(colormap=cm1)
plot(z, z, bg=cm1$zcol, pch=21, cex=1)
abline(h=0, lwd=0.5, col="magenta", lty="dashed")

z <- seq(-500, 500, 50)
cm2 <- colormap(z=z, name="gmt_globe", debug=debug)
par(mar=c(3,3,1,1))
drawPalette(colormap=cm2)
plot(z, z, bg=cm2$zcol, pch=21, cex=1)
abline(h=0, lwd=0.5, col="magenta", lty="dashed")

cm3 <- colormap(z=z, name="gmt_globe", debug=debug)
par(mar=c(3,3,1,1))
drawPalette(colormap=cm3, zlim=c(-500, 0), debug=debug)
plot(z, z, bg=cm3$zcol, pch=21, cex=1)
abline(h=0, lwd=0.5, col="magenta", lty="dashed")
mtext("named case, with zlim to narrow focus", cex=par("cex"))

cm4 <- colormap(z=z, zlim=c(-1000,0))
par(mar=c(3,3,1,1))
drawPalette(colormap=cm4, zlim=c(-200, 0))
plot(z, z, bg=cm4$zcol, pch=21, cex=1)
abline(h=0, lwd=0.5, col="magenta", lty="dashed")
mtext("non-named case with zlim to narrow focus", cex=par("cex"))



