## Try using colorRamp() to devise colours
library(oce)
bias <- 1

land <- oceColorsGebco(region="land", n=9)
water <- oceColorsGebco(region="water", n=9)
n <- 255

if (!interactive())
    png("topo_colors_02.png", height=4, width=7, unit="in", res=200, pointsize=12)
par(mfrow=c(1, 3))
par(mar=c(3,3,2,1), mgp=c(2,0.7,0))
z <- seq(-300, 300, length.out=n)
m <- matrix(z, nrow=1)


## Existing scheme (with white in between)
rgb.list <- col2rgb(c(water, "#FFFFFF", land)) / 255
l <- ncol(rgb.list)
r <- approx(1:l, rgb.list[1, 1:l], xout=seq(1, l, length.out=n))$y
g <- approx(1:l, rgb.list[2, 1:l], xout=seq(1, l, length.out=n))$y
b <- approx(1:l, rgb.list[3, 1:l], xout=seq(1, l, length.out=n))$y
col <- rgb(r, g, b)
imagep(1, z, matrix(z,nrow=1), col=col)
abline(h=seq(-300, 300, 50), lwd=0.25)
mtext("white band & old interpolation", side=3, line=0, cex=par("cex"))

for (space in c("rgb", "Lab")) {
    cr <- colorRamp(c(water, land), bias=1, space=space, interpolate="spline")
    col <- rgb(cr(seq(0, 1, length.out=n))/255)
    imagep(1, z, matrix(z,nrow=1), col=col)
    abline(h=seq(-300, 300, 50), lwd=0.25)
    mtext(paste("spline interp. in ", space, "space"), side=3, line=0, cex=par("cex"))
}

if (!interactive())
    dev.off()
