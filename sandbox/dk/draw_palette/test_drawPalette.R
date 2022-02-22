library(oce)
e <- function(msg)
{
    plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
    text(0.5, 0.5, msg)
    par(mar=c(3,3,1,1))
}

if (!interactive())
    pdf("test_drawPalette.pdf")

drawPalette(c(0,10))
e("Expect smooth gradation")

drawPalette(c(0,10), breaks=seq(0,10,1))
e("Expect colour shifts at 1, 2, ...")

drawPalette(c(0,10), breaks=seq(0,10,1), drawContours=TRUE)
e("Expect colour shifts and lines at 1, 2, ...")

if (!interactive())
    dev.off()

