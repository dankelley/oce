library(oce)
par(mar=c(3,3,1,1), mfrow=c(2,1))
z <- seq(-10e3, 10e3, 250)
cm <- colormap(z=z, name="gmt_globe", debug=2)
drawPalette(colormap=cm)
plot(z, z, bg=cm$zcol, pch=21, cex=1)
abline(h=0, lwd=0.5, col="magenta", lty="dashed")

z <- seq(-200, 200, 25)
cm <- colormap(z=z, name="gmt_globe", debug=2)
drawPalette(colormap=cm)
plot(z, z, bg=cm$zcol, pch=21, cex=1)
abline(h=0, lwd=0.5, col="magenta", lty="dashed")

