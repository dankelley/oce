library(oce) # must be recent, containing parametericDerivative()
library(Rcpp)

#' Curvature of parameteric function
#'
#' @param x,y Numeric vectors
#' @references
#' https://en.wikipedia.org/wiki/Curvature#Local_expressions
curvature <- function(x, y)
{
    n <- length(x)
    if (n > 10) {
        ##plot(seq_along(x), x)
        x <- predict(smooth.spline(seq_along(x), x), df=4)$y
        ##lines(seq_along(x), x)
        y <- predict(smooth.spline(seq_along(y), y, df=4))$y
    }
    Dx <- parametricDerivative(x, "NA")
    Dy <- parametricDerivative(y, "NA")
    DDx <- parametricDerivative(Dx, "NA")
    DDy <- parametricDerivative(Dy, "NA")
    list(curvature=(Dx*DDy-Dy*DDx) / (Dx^2+Dy^2)^(3/2),
         Dx=Dx, Dy=Dy)
}

n <- 21
x0 <- 0.5
y0 <- 0.5
x <- seq(0, 1, length.out=n)
y <- seq(0, 1, length.out=n)
## set.seed(2019.0212)
z <- outer(x, y, function(x,y) 1.5*(x-x0)^2+(y-y0)^2) + rnorm(n*n, sd=0.005/10000)
levels <- seq(0.05, 1, 0.05)
if (!interactive()) pdf("contourlabels_01.pdf")
par(mar=c(2, 2, 1, 1))
contour(x, y, z, levels=levels, labcex=1)
##.stop()

## Labelling at lowest curvature spots
levels <- 0.05
for (level in levels) {
    a <- contourLines(x, y, z, levels=level)
    for (i in seq_along(a)) {
        xc <- a[[i]]$x
        yc <- a[[i]]$y
        ##lines(xc, yc, col=2, type='o', pch=20, cex=2/3)
        lines(xc, yc, col=2, pch=20, cex=2/3)
        if (length(xc) > 10) {
            C <- curvature(xc, yc)
            m <- which.min(abs(C$curvature))#, na.rm=TRUE)
            if (length(m)) {
                ##> message("level=",level," i=",i, " m=", m)
                xl <- xc[m]
                yl <- yc[m]
                angle <- 180/pi*atan2(C$Dy[m], C$Dx[m])
                if (angle > 90)
                    angle <- angle - 180
                label <- as.character(level)
                w <- 1.1*strwidth(level, "user")
                h <- 1.1*strheight(level, "user")
                S <- sin(-angle*pi/180)
                C <- cos(-angle*pi/180)
                rot <- matrix(c(C, -S, S, C), byrow=TRUE, nrow=2)
                X <- c(-w/2, -w/2, w/2, w/2)
                Y <- c(-h/2, h/2, h/2, -h/2)
                XY <- cbind(X, Y)
                XYrot <- XY %*% rot
                polygon(xl+XYrot[,1], yl+XYrot[,2], col="white", border="white")
                text(xl, yl, level, srt=angle, col=2)
            }
        }
    }
}
if (!interactive()) dev.off()

