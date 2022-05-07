library(oce)
data(coastlineWorldFine, package="ocedata")
clat <- 45
clon <- -64
plot(coastlineWorldFine, clongitude=clon, clatitude=clat, span=300,
    mar=c(2,2,2,1))
n <- 9
x <- rep(clon, length.out=n)
y <- rep(clat, length.out=n)
theta <- seq(0, 2*pi, length.out=n)
u <- cos(theta)
v <- sin(theta)
drawDirectionField(x, y, u, v, scalex=1, add=TRUE, col=2, lwd=3)
mtext("Q: are red lines at 90deg and 45deg?", col=2, line=0.5, font=2)

