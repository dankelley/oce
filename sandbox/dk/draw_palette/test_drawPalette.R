library(oce)
t <- 1
f <- function(e, msg="", cex=0.8)
{
    E <- deparse(substitute(e))
    eval(e)
    plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
    text(0.5, 0.6, paste("Test", t), cex=cex)
    text(0.5, 0.5, E, cex=cex)
    text(0.5, 0.4, msg, cex=cex)
    par(mar=c(3,2,2,1))
    t <<- t + 1
}

if (!interactive())
    pdf("test_drawPalette.pdf")
par(mar=c(3,3,2,1))

f(drawPalette(zlim=c(0,20)),
    "Expect: continous gradient")
f(drawPalette(zlim=c(0,20), breaks=seq(0,20,1)),
    "Expect: stepped gradient")
f(drawPalette(zlim=c(0,20), breaks=seq(0,20,1), drawContours=TRUE),
    "Expect: stepped with lines")

if (!interactive())
    dev.off()

