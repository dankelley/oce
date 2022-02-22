library(oce)
t <- 1
f <- function(e, msg="", cex=0.8)
{
    E <- paste(strwrap(deparse(substitute(e)), 50), collapse="\n")
    eval(e)
    plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
    text(0.5, 0.9, paste("Test", t), cex=cex, font=2)
    text(0.5, 0.8, msg, cex=cex, font=2)
    text(0.5, 0.6, E, cex=cex)
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
f(drawPalette(zlim=c(0,20), breaks=c(0, 2, 5, 15, 20),
              at=c(0, 17, 20), labels = as.character(c(0, 17, 20))),
    "Expect: breaks at 0, 2, 5, 15, 20 with labels at 0, 17, 20")

if (!interactive())
    dev.off()

