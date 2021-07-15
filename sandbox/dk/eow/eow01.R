rm(list=ls())
library(oce)
data(coastlineWorld)
projs <- c("+proj=ortho",
           "+proj=robin",
           "+proj=moll")
nprojs <- length(projs)
par(mar=c(1,1,2,1))
par(mfrow=c(1,3))
for (proj in projs) {
    mapPlot(coastlineWorld, proj=proj, col="lightgray")
    mtext(proj)
    usr <- par("usr")
    x0 <- mean(usr[1:2])
    y0 <- mean(usr[3:4])
    f <- function(r)
    {
        x <- x0 + r*cos(theta)
        y <- y0 + r*sin(theta)
        ifelse(is.na(map2lonlat(x, y)$longitude), -1, 1)
    }
    n <- 32
    X <- Y <- vector("numeric", n)
    i <- 1
    for (theta in seq(0, 2*pi, length.out=n)) {
        R <- max(c(usr[2]-usr[1], usr[4]-usr[3]))
        r <- uniroot(f, c(0, R))$root
        X[i] <- r*cos(theta)
        Y[i] <- r*sin(theta)
        i <- i + 1
        lines(x0+c(0,r*cos(theta)), y0+c(0,r*sin(theta)), col=4)
    }
    lines(X, Y, col="magenta")
}
